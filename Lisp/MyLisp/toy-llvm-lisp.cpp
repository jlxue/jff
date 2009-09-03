/* from http://paste.lisp.org/display/74068 */


/* A Trivial LLVM LISP
 * Copyright (C) 2008-2009 David Robillard <dave@drobilla.net>
 *
 * Parts from the Kaleidoscope tutorial <http://llvm.org/docs/tutorial/>
 * by Chris Lattner and Erick Tryzelaar
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with This program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <iostream>
#include <list>
#include <map>
#include <stack>
#include <string>
#include <vector>
#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Module.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Transforms/Scalar.h"

using namespace llvm;
using namespace std;


/***************************************************************************
 * S-Expression Lexer - Read text and output nested lists of strings       *
 ***************************************************************************/

struct SExp {
	SExp()                         : type(LIST)          {}
	SExp(const std::list<SExp>& l) : type(LIST), list(l) {}
	SExp(const std::string&     s) : type(ATOM), atom(s) {}
	enum { ATOM, LIST } type;
	std::string         atom;
	std::list<SExp>     list;
};

struct SyntaxError : public std::exception {
	SyntaxError(const char* m) : msg(m) {}
	const char* what() const throw() { return msg; }
	const char* msg;
};

static SExp
readExpression(std::istream& in)
{
	stack<SExp> stk;
	string      tok;

#define APPEND_TOK() \
	if (stk.empty()) return tok; else stk.top().list.push_back(SExp(tok))

	while (char ch = in.get()) {
		switch (ch) {
		case EOF:
			return SExp();
		case ' ': case '\t': case '\n':
			if (tok == "")
				continue;
			else
				APPEND_TOK();
			tok = "";
			break;
		case '"':
			do { tok.push_back(ch); } while ((ch = in.get()) != '"');
			tok.push_back('"');
			APPEND_TOK();
			tok = "";
			break;
		case '(':
			stk.push(SExp());
			break;
		case ')':
			switch (stk.size()) {
			case 0:
				throw SyntaxError("Missing '('");
				break;
			case 1:
				if (tok != "") stk.top().list.push_back(SExp(tok));
				return stk.top();
			default:
				if (tok != "") stk.top().list.push_back(SExp(tok));
				SExp l = stk.top();
				stk.pop();
				stk.top().list.push_back(l);
			}
			tok = "";
			break;
		default:
			tok.push_back(ch);
		}
	}

	switch (stk.size()) {
	case 0:  return tok;       break;
	case 1:  return stk.top(); break;
	default: throw SyntaxError("Missing ')'");
	}
}



/***************************************************************************
 * Abstract Syntax Tree                                                    *
 ***************************************************************************/

struct CEnv; ///< Compile Time Environment

/// Base class for all AST nodes.
struct AST {
	virtual ~AST() {}
	virtual Value* Codegen(CEnv& cenv) = 0;
	virtual bool evaluatable() const { return true; }
};

/// Numeric literals, e.g. "1.0".
struct ASTNumber : public AST {
	ASTNumber(double val) : _val(val) {}
	virtual Value* Codegen(CEnv& cenv);
private:
	double _val;
};

/// Variable reference (i.e. symbol), e.g. "a".
struct ASTVariable : public AST {
	ASTVariable(const string& name) : _name(name) {}
	virtual Value* Codegen(CEnv& cenv);
private:
	string _name;
};

/// Function call, e.g. "(func arg1 arg2)".
struct ASTCall : public AST {
	ASTCall(const string& f, vector<AST*>& a) : _func(f), _args(a) {}
	virtual Value* Codegen(CEnv& cenv);
private:
	string       _func;
	vector<AST*> _args;
};

/// Conditional (special form "if"), e.g. "(if cond thenexp elseexp)".
struct ASTIf : public AST {
	ASTIf(AST* c, AST* t, AST* e) : _cond(c), _then(t), _else(e) {}
	virtual Value* Codegen(CEnv& cenv);
private:
	AST* _cond;
	AST* _then;
	AST* _else;
};

/// Function prototype
struct ASTPrototype : public AST {
	ASTPrototype(bool foreign, const string& n, const vector<string>& a)
			: _foreign(foreign), _name(n), _args(a) {}
	virtual bool evaluatable() const { return false; }
	Value*       Codegen(CEnv& cenv) { return Funcgen(cenv); }
	Function*    Funcgen(CEnv& cenv);
private:
	bool           _foreign;
	string         _name;
	vector<string> _args;
};

/// Function definition
struct ASTFunction : public AST {
	ASTFunction(ASTPrototype* p, AST* b) : _proto(p), _body(b) {}
	virtual bool evaluatable() const { return false; }
	Value*       Codegen(CEnv& cenv) { return Funcgen(cenv); }
	Function*    Funcgen(CEnv& cenv);
private:
	ASTPrototype* _proto;
	AST*          _body;
};



/***************************************************************************
 * Parser - Transform S-Expressions into AST nodes                         *
 ***************************************************************************/

static const std::string& head(const SExp& exp)
{
	static const std::string empty = "";
	if (exp.type == SExp::LIST
			&& !exp.list.empty()
			&& exp.list.front().type == SExp::ATOM)
		return exp.list.front().atom;
	else
		return empty;
}

static AST* parseExpression(const SExp& exp);

/// identifierexpr ::= identifier
static AST* parseIdentifierExpr(const SExp& exp)
{
	assert(exp.type == SExp::ATOM);
	return new ASTVariable(exp.atom);
}

/// numberexpr ::= number
static AST* parseNumberExpr(const SExp& exp)
{
	assert(exp.type == SExp::ATOM);
	return new ASTNumber(strtod(exp.atom.c_str(), NULL));
}

/// ifexpr ::= ("if" expression expression expression)
static AST* parseIfExpr(const SExp& exp)
{
	assert(head(exp) == "if");
	list<SExp>::const_iterator i = exp.list.begin();
	++i;
	
	AST* cond      = parseExpression(*i++);
	AST* then      = parseExpression(*i++);
	AST* otherwise = parseExpression(*i++);

	return new ASTIf(cond, then, otherwise);
}

/// callexpr ::= (expression [...])
static AST* parseCallExpr(const SExp& exp)
{
	assert(head(exp) != "");
	list<SExp>::const_iterator i = exp.list.begin();
	const string& name = i->atom;

	vector<AST*> params;
	for (++i; i != exp.list.end(); ++i)
		params.push_back(parseExpression(*i));

	return new ASTCall(name, params);
}

/// prototype ::= (name [arg*])
static ASTPrototype* ParsePrototype(bool foreign, const SExp& exp)
{
	assert(head(exp) != "");
	list<SExp>::const_iterator i = exp.list.begin();
	const string& name = i->atom;
	
	vector<string> args;
	for (++i; i != exp.list.end(); ++i)
		if (i->type == SExp::ATOM)
			args.push_back(i->atom);
		else
			throw SyntaxError("Expected parameter name, found list");

	return new ASTPrototype(foreign, name, args);
}

/// definition ::= ("def" prototype expression)
static ASTFunction* parseDefinition(const SExp& exp)
{
	assert(head(exp) == "def");
	list<SExp>::const_iterator i = exp.list.begin();
	++i;

	ASTPrototype* proto = ParsePrototype(false, *i++);
	AST*          body  = parseExpression(*i++);
	
	return new ASTFunction(proto, body);
}

/// foreign ::= ("foreign" prototype expression)
static ASTPrototype* parseForeign(const SExp& exp)
{
	assert(head(exp) == "foreign");
	list<SExp>::const_iterator i = exp.list.begin();
	++i;

	return ParsePrototype(true, *i++);
}

static AST* parseExpression(const SExp& exp)
{
	if (exp.type == SExp::LIST) {
		const string& form = head(exp);
		if (form == "if") {
			return parseIfExpr(exp);
		} else if (form == "def") {
			return parseDefinition(exp);
		} else if (form == "foreign") {
			return parseForeign(exp);
		} else {
			return parseCallExpr(exp);
		}
	} else if (isalpha(exp.atom[0])) {
		return parseIdentifierExpr(exp);
	} else if (isdigit(exp.atom[0])) {
		return parseNumberExpr(exp);
	} else {
		throw SyntaxError("Illegal atom");
	}
}


/***************************************************************************
 * Code Generation                                                         *
 ***************************************************************************/

/// Compile-time environment
struct CEnv {
	CEnv(Module* m, const TargetData* target)
		: module(m), provider(module), fpm(&provider)
	{
		// Set up the optimizer pipeline.
		// Register info about how the target lays out data structures.
		fpm.add(new TargetData(*target));
		// Do simple "peephole" and bit-twiddline optimizations.
		fpm.add(createInstructionCombiningPass());
		// Reassociate expressions.
		fpm.add(createReassociatePass());
		// Eliminate Common SubExpressions.
		fpm.add(createGVNPass());
		// Simplify control flow graph (delete unreachable blocks, etc).
		fpm.add(createCFGSimplificationPass());
	}
	IRBuilder<>            builder;
	Module*                module;
	ExistingModuleProvider provider;
	FunctionPassManager    fpm;
	map<string, Value*>    env;
};

Value* ASTNumber::Codegen(CEnv& cenv)
{
	return ConstantFP::get(APFloat(_val));
}

Value* ASTVariable::Codegen(CEnv& cenv)
{
	map<string, Value*>::const_iterator v = cenv.env.find(_name);
	if (v == cenv.env.end()) throw SyntaxError("Undefined identifier");
	return v->second;
}

Value* ASTCall::Codegen(CEnv& cenv)
{
	// Look up the name in the global module table.
	Function* f = cenv.module->getFunction(_func);
	if (f == 0) throw SyntaxError("Undefined function");
	if (f->arg_size() != _args.size()) throw SyntaxError("Illegal arguments");

	vector<Value*> args;
	for (size_t i = 0, e = _args.size(); i != e; ++i)
		args.push_back(_args[i]->Codegen(cenv));

	return cenv.builder.CreateCall(f, args.begin(), args.end(), "calltmp");
}

Value* ASTIf::Codegen(CEnv& cenv)
{
	Value* condV = _cond->Codegen(cenv);

	// Convert condition to a bool by comparing equal to 0.0.
	condV = cenv.builder.CreateFCmpONE(
			condV, ConstantFP::get(APFloat(0.0)), "ifcond");

	Function* parent = cenv.builder.GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.
	// Insert the 'then' block at the end of the function.
	BasicBlock* thenBB = BasicBlock::Create("then", parent);
	BasicBlock* elseBB = BasicBlock::Create("else");
	BasicBlock* mergeBB = BasicBlock::Create("ifcont");

	cenv.builder.CreateCondBr(condV, thenBB, elseBB);

	// Emit then value.
	cenv.builder.SetInsertPoint(thenBB);
	Value* thenV = _then->Codegen(cenv);

	cenv.builder.CreateBr(mergeBB);
	// Codegen of 'Then' can change the current block, update thenBB
	thenBB = cenv.builder.GetInsertBlock();

	// Emit else block.
	parent->getBasicBlockList().push_back(elseBB);
	cenv.builder.SetInsertPoint(elseBB);
	Value* elseV = _else->Codegen(cenv);

	cenv.builder.CreateBr(mergeBB);
	// Codegen of 'Else' can change the current block, update elseBB
	elseBB = cenv.builder.GetInsertBlock();

	// Emit merge block.
	parent->getBasicBlockList().push_back(mergeBB);
	cenv.builder.SetInsertPoint(mergeBB);
	PHINode* pn = cenv.builder.CreatePHI(Type::DoubleTy, "iftmp");

	pn->addIncoming(thenV, thenBB);
	pn->addIncoming(elseV, elseBB);
	return pn;
}

Function* ASTPrototype::Funcgen(CEnv& cenv)
{
	// Make the function type, e.g. double(double,double)
	vector<const Type*> Doubles(_args.size(), Type::DoubleTy);
	FunctionType* FT = FunctionType::get(Type::DoubleTy, Doubles, false);

	Function* f = Function::Create(
			FT, Function::ExternalLinkage, _name, cenv.module);

	// If F conflicted, there was already something named 'Name'.
	// If it has a body, don't allow redefinition.
	if (f->getName() != _name) {
		// Delete the one we just made and get the existing one.
		f->eraseFromParent();
		f = cenv.module->getFunction(_name);

		// If F already has a body, reject this.
		if (!f->empty()) throw SyntaxError("Function redefined");

		// If F took a different number of args, reject.
		if (f->arg_size() != _args.size())
			throw SyntaxError("Function redefined with mismatched arguments");
	}

	// Set names for all arguments.
	size_t i = 0;
	for (Function::arg_iterator a = f->arg_begin(); i != _args.size();
	        ++a, ++i) {
		a->setName(_args[i]);

		// Add arguments to variable symbol table.
		cenv.env[_args[i]] = a;
	}

	return f;
}

Function* ASTFunction::Funcgen(CEnv& cenv)
{
	cenv.env.clear();

	Function* f = _proto->Funcgen(cenv);

	// Create a new basic block to start insertion into.
	BasicBlock* bb = BasicBlock::Create("entry", f);
	cenv.builder.SetInsertPoint(bb);

	try {
		Value* retVal = _body->Codegen(cenv);
		cenv.builder.CreateRet(retVal); // Finish function
		verifyFunction(*f); // Validate generated code
		cenv.fpm.run(*f); // Optimize function
		return f;
	} catch (SyntaxError e) {
		f->eraseFromParent(); // Error reading body, remove function
		throw e;
	}

	return 0; // Never reached
}


/***************************************************************************
 * REPL - Interactively compile, optimise, and execute code                *
 ***************************************************************************/

/// Read-Eval-Print-Loop
static void repl(CEnv& cenv, ExecutionEngine* engine)
{
	while (1) {
		std::cout << "> ";
		std::cout.flush();
		SExp exp = readExpression(std::cin);

		try {
			AST* ast = parseExpression(exp);
			if (ast->evaluatable()) {
				ASTPrototype* proto = new ASTPrototype(false, "", vector<string>());
				ASTFunction*  func  = new ASTFunction(proto, ast);
				Function*     code  = func->Funcgen(cenv);
				void*         fp    = engine->getPointerToFunction(code);
				double (*f)() = (double (*)())fp;
				std::cout << f() << endl;
			} else {
				Value* code = ast->Codegen(cenv); 
				std::cout << "Generated code:" << endl;
				code->dump();
			}
		} catch (SyntaxError e) {
			std::cerr << "Syntax error: " << e.what() << endl;
		}
	}
}


/***************************************************************************
 * Main driver code.
 ***************************************************************************/

int
main()
{
	Module*          module = new Module("interactive");
	ExecutionEngine* engine = ExecutionEngine::create(module);
	
	CEnv cenv(module, engine->getTargetData());

	repl(cenv, engine);

	std::cout << "Generated code:" << endl;
	module->dump();

	return 0;
}


