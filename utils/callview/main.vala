using GLib;
using CallView;

static int main(string[] args) {
    if (args.length == 1) {
        stderr.printf("Usage: callview objfile\n");
        return -1;
    }

    CModule module = new CModule(args[1]);
    module.load();

    return 0;
}

