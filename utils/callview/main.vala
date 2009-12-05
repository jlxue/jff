using GLib;
using CallView;

static int main(string[] args) {
    if (args.length == 1) {
        stderr.printf("Usage: callview objfile\n");
        return -1;
    }

    CObjectFile obj = new CObjectFile(args[1]);
    obj.parse();

    return 0;
}

