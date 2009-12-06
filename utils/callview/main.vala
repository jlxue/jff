using GLib;
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
    Array<Symbol> symbols = null;
    unowned Array<Callee> callees = null;
    bool search_callees = false;
    string file = null;

    for (;;) {
        stdout.printf(">> ");
        if (null == (input = stdin.read_line()))
            break;

        if (numbers.match(input, 0, null)) {
            if (! search_callees && (symbols == null || symbols.length == 0)) {
                stderr.printf("No symbols found, input a search pattern please!\n");
                continue;
            }

            if (search_callees && (callees == null || callees.length == 0)) {
                stderr.printf("No callees found, input a number or search pattern please!\n");
                continue;
            }

            uint n = (uint)input.to_long();

            if (! search_callees && (n < 1 || n > symbols.length)) {
                stderr.printf("Bad number, valid range is 1 - %u\n", symbols.length);
                continue;
            }

            if (search_callees && (n < 1 || n > callees.length)) {
                stderr.printf("Bad number, valid range is 1 - %u\n", callees.length);
                continue;
            }

            if (! search_callees) {
                callees = module.findCallees(symbols.index(n - 1));
                file = symbols.index(n - 1).file;
            } else {
                callees = module.findCallees(callees.index(n - 1).symbol);
                file = callees.index(n - 1).symbol.file;
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
            stdout.printf("callview: %u lines\n", callees.length);
            for (uint i = 0; i < callees.length; ++i) {
                Callee callee = callees.index(i);
                stdout.printf("%3u %s:%u %s\n",
                              i + 1,
                              callee.file != null ?  callee.file : file,
                              callee.line,
                              callee.symbol.signature);
            }
        } else {
            stdout.printf("callview: %u lines\n", symbols.length);
            for (uint i = 0; i < symbols.length; ++i) {
                Symbol sym = symbols.index(i);
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

