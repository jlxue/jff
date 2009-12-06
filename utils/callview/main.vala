using GLib;
using CallView;

static int main(string[] args) {
    if (args.length != 3 ) {
        stderr.printf("Usage: callview objfile function\n");
        return -1;
    }

    CModule module = new CModule(args[1]);
    module.load();

    Array<Symbol> symbols = module.findSymbol(new Regex("main"));
    stdout.printf("symbols.length=%u\n", symbols.length);

    unowned Array<Callee> callees = module.queryCallees(symbols.index(0));
    if (null != callees) {
        stdout.printf("callees.length=%u\n", callees.length);
    }

    return 0;
}

