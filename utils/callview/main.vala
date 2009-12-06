using Gee;
using CallView;

static int main(string[] args) {
    if (args.length != 2 ) {
        stderr.printf("Usage: callview objfile\n");
        return -1;
    }

    CModule module = new CModule(args[1]);
    module.load();


    Regex numbers;

    try {
        numbers = new Regex("""^\d+$""", 0, 0);
    } catch (RegexError e) {
        stderr.printf("Bad regex: %s\n", e.message);
        return -1;
    }


    string input;
    Gee.List<Symbol> symbols = null;
    Gee.List<Callee> callees = null;
    bool search_callees = false;
    string file = null;

    for (;;) {
        stdout.printf(">> ");
        if (null == (input = stdin.read_line()))
            break;

        if (input.length == 0) {
            continue;
        }

        if (numbers.match(input, 0, null)) {
            if (! search_callees && (symbols == null || symbols.size == 0)) {
                stderr.printf("No symbols found, input a search pattern please!\n");
                continue;
            }

            if (search_callees && (callees == null || callees.size == 0)) {
                stderr.printf("No callees found, input a number or search pattern please!\n");
                continue;
            }

            int n = (int)input.to_long();

            if (! search_callees && (n < 1 || n > symbols.size)) {
                stderr.printf("Bad number, valid range is 1 - %u\n", symbols.size);
                continue;
            }

            if (search_callees && (n < 1 || n > callees.size)) {
                stderr.printf("Bad number, valid range is 1 - %u\n", callees.size);
                continue;
            }

            if (! search_callees) {
                Symbol sym = symbols.get(n - 1);
                callees = module.findCallees(sym);
                file = sym.file;
            } else {
                Callee callee = callees.get(n - 1);
                callees = module.findCallees(callee.symbol);
                file = callee.symbol.file;
            }

            search_callees = true;
        } else {
            Regex regex;

            try {
                regex = new Regex(input, RegexCompileFlags.CASELESS, 0);
            } catch (RegexError e) {
                stderr.printf("Bad regex: %s\n", e.message);
                continue;
            }

            symbols = module.findSymbols(regex);

            search_callees = false;
        }

        if (search_callees) {
            stdout.printf("callview: %u lines\n", callees.size);
            for (int i = 0; i < callees.size; ++i) {
                Callee callee = callees.get(i);
                stdout.printf("%3u %s:%u %s\n",
                              i + 1,
                              callee.file != null ?  callee.file : file,
                              callee.line,
                              callee.symbol.signature);
            }
        } else {
            stdout.printf("callview: %u lines\n", symbols.size);
            for (int i = 0; i < symbols.size; ++i) {
                Symbol sym = symbols.get(i);
                stdout.printf("%3u %s:%u %s\n",
                              i + 1,
                              sym.file,
                              sym.line,
                              sym.signature);
            }
        }
    }


    return 0;
}

