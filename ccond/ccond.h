/*
 * Copyright (c) 2008, Liu Yubao.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials
 *     provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Purpose
 * ~~~~~~~
 *  A portable exception handling library for C and C++ inspired by the
 *  condition system in Common Lisp, see "Practical Common Lisp" by Peter
 *  Seibel chapter 19 "Beyond Exception Handling - Conditions and Restarts"
 *  for details.
 *
 * Author
 * ~~~~~~
 *  Liu Yubao <yubao.liu@gmail.com>
 *
 * Version
 * ~~~~~~~
 *  0.2
 *
 * Usage
 * ~~~~~
 *  1) building
 *
 *  For C:
 *      gcc -o app app.c ccond.c                (GCC)
 *      cl /Feapp app.c ccond.c                 (MS VC > 6.0)
 *
 *  For C++:
 *      g++ -o app app.cpp ccond.c              (GCC)
 *      cl /Feapp /TP /EHsc app.cpp ccond.c     (MS VC > 6.0)
 *
 *  Macros:
 *      ! defined(NDEBUG)       enable assertion and stack overflow detection
 *                              (enabled by default)
 *      defined(CCOND_DEBUG)    enable more debugging information
 *                              (disabled by default)
 *      CCOND_ENABLE_THREAD_SAFE    whether ccond is thread-safe
 *                      0       not thread-safe (default)
 *                      1       is thread-safe
 *      CCOND_ENABLE_CXX_EXCEPTION      only for C++
 *                      0   don't use C++ exception, objects on stack won't be
 *                          destroyed automatically when signal a condition
 *                      1   use C++ exception (default)
 *      STACK_INCREASE_TO_LOW_ADDR
 *                      0   stack increase from low address to high address
 *                      1   stack increase from high address to low address
 *                          (default)
 *      defined(HAVE_EXEC_INFO_H)
 *                          have execinfo.h, support backtrace output
 *                          (disabled by default)
 *
 *  2) coding
 *
 *      (a) restart-case clause
 *      ~~~~~~~~~~~~~~~~~~~~~~~
 *
 *      RESTART_CASE_BEGIN("restart1", "restart2") {
 *      #if CCOND_ENABLE_CXX_EXCEPTION
 *      case 0:
 *          ... some code that can signal condition...
 *          break;
 *      #else
 *          ... some code that can signal condition...
 *      } RESTART_CATCH {
 *      #endif
 *      case 1:
 *          ... code for restart 1...
 *          ... use RESTART_ARG(type) to get argument of a restart...
 *          ... For example: char* message = RESTART_ARG(char*);  ...
 *          break;
 *      case 2:
 *          ... code for restart 2...
 *          break;
 *      default:
 *          assert(0);
 *      } RESTART_CASE_END;
 *
 *      // You'd better use "const char*" to define restart name.
 *
 *
 *      (b) handler-bind clause
 *      ~~~~~~~~~~~~~~~~~~~~~~~
 *
 *      HANDLER_BIND_BEGIN({"condition1", handler1}, {"condition2", handler2}) {
 *          ...some code that can signal condition...
 *      } HANDLER_BIND_END;
 *
 *      // You'd better use "const char*" to define condition name.
 *
 *      !!! CAUTION !!!
 *      !!! MUSTN'T escape from restart-case and handler-bind clauses
 *      !!! with goto, longjmp/siglongjmp or throw.
 *      !!! DON't catch ccond_exception yourself.
 *
 *
 *      (c) extends Condition
 *      ~~~~~~~~~~~~~~~~~~~~
 *
 *      typedef struct {
 *          EXTENDS_CONDITION;      // must be the first member!
 *          char*   message;
 *          int     age;
 *          char*   name;
 *      } MyCondition;
 *
 *      Use "_name" member to access the name of a condition.
 *
 *
 *      (d) functions
 *      ~~~~~~~~~~~~~
 *
 *      ccond_init()        - initialize ccond for *current thread*
 *      ccond_error()       - like function "error" in Common Lisp
 *      ccond_signal()      - like function "signal" in Common Lisp
 *      find_restart()      - like function "find_restart" in Common Lisp
 *      invoke_restart()    - like function "invoke_restart" in Common Lisp
 *
 *
 * Todo
 * ~~~~
 *  - implement unwind_protect
 *  - more tests needed!!!
 *
 * ChangeLog
 * ~~~~~~~~~
 *  2008-12-21  Liu Yubao
 *      - initial implementation, not thread-safe, v0.1
 *
 *  2008-12-23  Liu Yubao
 *      - use thread local storage, now ccond is thread-safe, v0.2
 *
 */
#ifndef CCOND_H__
#define CCOND_H__

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {

#ifndef CCOND_ENABLE_CXX_EXCEPTION
#define CCOND_ENABLE_CXX_EXCEPTION  1
#endif

#else
#define CCOND_ENABLE_CXX_EXCEPTION  0
#endif  /* __cplusplus  */

/*---------------------------- macro definition ----------------------------*/
#ifndef STACK_INCREASE_TO_LOW_ADDR
#define STACK_INCREASE_TO_LOW_ADDR  1
#endif

#ifndef CCOND_ENABLE_THREAD_SAFE
#define CCOND_ENABLE_THREAD_SAFE    0
#endif


#if     CCOND_ENABLE_THREAD_SAFE

#if defined(__INTEL_COMPILER) || defined(__ICL) || defined(__ICC) || defined(__ECC)
#define CCOND_ICC
#endif

#if defined(_WIN32) || defined(_WIN64) || defined(_WINDOWS)
#define CCOND_WIN
#endif

#if defined(__SUNPRO_CC) || defined(__GNUC__) || (defined(CCOND_ICC) && !defined(CCOND_WIN))
#define CCOND_TLS   __thread
#else
#define CCOND_TLS   __declspec(thread)
#endif

#else
#define CCOND_TLS
#endif  /* CCOND_ENABLE_THREAD_SAFE */


#if defined(_POSIX_C_SOURCE) || defined(_XOPEN_SOURCE)
#define CCOND_JMP_BUF               sigjmp_buf
#define CCOND_SETJMP(env)           sigsetjmp((env), 1)
#define CCOND_LONGJMP(env, val)     siglongjmp((env), (val))
#else
#define CCOND_JMP_BUF               jmp_buf
#define CCOND_SETJMP(env)           setjmp(env)
#define CCOND_LONGJMP(env, val)     longjmp((env), (val))
#endif

#if STACK_INCREASE_TO_LOW_ADDR
#define     CCOND_STACK_CONTAIN(outer, inner)                           \
    (NULL == (outer) || (void*)(outer) > (void*)(inner))
#else
#define     CCOND_STACK_CONTAIN(outer, inner)                           \
    (NULL == (outer) || (void*)(outer) < (void*)(inner))
#endif


#ifndef NDEBUG
#define CCOND_DECL_HEAD_GUARD   unsigned int head_guard;
#define CCOND_DECL_TAIL_GUARD   unsigned int tail_guard;
#define CCOND_SET_GUARD(r)      (r).head_guard = 0xdeadbeef;            \
                                (r).tail_guard = 0xbeefdead
#else
#define CCOND_DECL_HEAD_GUARD
#define CCOND_DECL_TAIL_GUARD
#define CCOND_SET_GUARD(r)
#endif

#define CCOND_CHECK_GUARD(r)    assert((r).head_guard == 0xdeadbeef &&  \
                                       (r).tail_guard == 0xbeefdead)


#ifdef  CCOND_DEBUG
#define CCOND_DECL_FILE_LINE    const char* file; int line;
#define CCOND_SET_FILE_LINE(r)  (r).file = __FILE__; (r).line = __LINE__
#else
#define CCOND_DECL_FILE_LINE
#define CCOND_SET_FILE_LINE(r)
#endif


#define     RESTART_CASE_BEGIN(...)                                     \
    do {                                                                \
        RestartCase     _r__;                                           \
        const char*     _names__[] = {                                  \
            __VA_ARGS__, NULL                                           \
        };                                                              \
        CCOND_SET_GUARD(_r__);                                          \
        CCOND_SET_FILE_LINE(_r__);                                      \
        _r__.names = _names__;                                          \
        _r__.prev  = _last_restart_case;                                \
        _last_restart_case = &_r__;                                     \
        assert(CCOND_STACK_CONTAIN(_r__.prev, _last_restart_case) &&    \
                "RESTART_CASE_END missed or bypassed!");                \
                                                                        \
        RESTART_TRY


#if CCOND_ENABLE_CXX_EXCEPTION
#define     RESTART_TRY     try

#define     RESTART_CATCH                                               \
        catch (ccond_exception& e) {                                    \
            if (e._r != &_r__)                                          \
                throw;                                                  \
            switch(_r__.val)

#define     RESTART_CATCH_END       }

#else

#define     RESTART_TRY     switch (CCOND_SETJMP(_r__.env))

#define     RESTART_CATCH                                               \
        "C++ is evil, RESTART_CATCH is only required in C++."

#define     RESTART_CATCH_END

#endif


#define     RESTART_CASE_END                                            \
        RESTART_CATCH_END                                               \
        _last_restart_case = _r__.prev;                                 \
    } while (0)


#define     HANDLER_BIND_BEGIN(...)                                     \
    do {                                                                \
        HandlerBind     _h__;                                           \
        HandlerPair     _binds__[] = {                                  \
            __VA_ARGS__, {NULL, NULL}                                   \
        };                                                              \
        CCOND_SET_GUARD(_h__);                                          \
        CCOND_SET_FILE_LINE(_h__);                                      \
        _h__.binds = _binds__;                                          \
        _h__.prev  = _last_handler_bind;                                \
        _last_handler_bind = &_h__;                                     \
        assert(CCOND_STACK_CONTAIN(_h__.prev, _last_handler_bind) &&    \
                "HANDLER_BIND_END missed or bypassed!");


#define     HANDLER_BIND_END                                            \
        _last_handler_bind = _h__.prev;                                 \
    } while (0)


#define     RESTART_ARG(type)   ((type)(_r__.arg))


#define     EXTENDS_CONDITION                                           \
    struct {                                                            \
        const char*     _name;                                          \
    }

/*----------------------------- type definition ----------------------------*/
typedef EXTENDS_CONDITION Condition;

typedef void (*Handler)(Condition* c);

typedef struct {
    const char*         name;       /* name of a condition                  */
    Handler             handler;    /* handler for a condition              */
} HandlerPair;

struct HandlerBind {
    CCOND_DECL_HEAD_GUARD
    struct HandlerBind* prev;       /* previous handler-bind clause         */
    HandlerPair*        binds;      /* pairs of (condition_name, handler)   */
    CCOND_DECL_FILE_LINE
    CCOND_DECL_TAIL_GUARD
};

typedef struct HandlerBind  HandlerBind;

struct RestartCase {
    CCOND_DECL_HEAD_GUARD
    struct RestartCase* prev;   /* previous restart-case clause             */
#if ! CCOND_ENABLE_CXX_EXCEPTION
    CCOND_JMP_BUF       env;    /* for setjmp() and longjmp()               */
#endif
    int                 val;    /* for longjmp(), means which restart name  */
    void*               arg;    /* extra argument when calling a restart    */
    const char**        names;  /* restart names in a restart-case clause   */
    CCOND_DECL_FILE_LINE
    CCOND_DECL_TAIL_GUARD
};

typedef struct RestartCase  RestartCase;

#if CCOND_ENABLE_CXX_EXCEPTION
class ccond_exception {
public:
    const RestartCase*  _r;
    ccond_exception(const RestartCase* r) : _r(r) {}
};
#endif

/*---------------------------- external declaration ------------------------*/
extern CCOND_TLS    HandlerBind*    _last_handler_bind;
extern CCOND_TLS    RestartCase*    _last_restart_case;

/*---------------------------- function prototype --------------------------*/
int ccond_init(void);

void ccond_signal(Condition *c);

#ifdef CCOND_DEBUG
void ccond_error(Condition* c, const char* file, int line);
RestartCase* find_restart(const char* name, const char* file, int line);
#define ccond_error(c)      ccond_error((c), __FILE__, __LINE__)
#define find_restart(s)     find_restart((s), __FILE__, __LINE__)
#else
void ccond_error(Condition* c);
RestartCase* find_restart(const char* name);
#endif

void invoke_restart(RestartCase* r, void* arg /* can't point into stack */)
#if CCOND_ENABLE_CXX_EXCEPTION
    throw(ccond_exception);
#else
    ;
#endif


#ifdef __cplusplus
}
#endif

#endif  /* CCOND_H__ */

