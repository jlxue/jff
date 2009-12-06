using GLib;

namespace CallView {

public delegate bool LineProcessor(string line);

public delegate void CSymbolProcessor(uint address, uint size,
                                      string signature,
                                      string? file, uint line);


public static bool pipe(string[] argv, string[]? envp,
                        LineProcessor processor) {
    bool ret;
    Pid child_pid;
    int output_fd;

    try {
        ret = Process.spawn_async_with_pipes(null, argv, envp,
                                             SpawnFlags.SEARCH_PATH, null,
                                             out child_pid,
                                             null, out output_fd);
    } catch (SpawnError e) {
        stderr.printf("pipe error: %s \n", e.message);
        return false;
    }

    if (! ret) {
        stderr.printf("bad pipe!");
        return false;
    }

    IOChannel io = new IOChannel.unix_new(output_fd);
    StringBuilder sb = new StringBuilder();
    size_t terminator_pos;
    IOStatus status;

    for (;;) {
        try {
            status = io.read_line_string(sb, out terminator_pos);
        } catch (ConvertError e) {
            stderr.printf("convert error: %s\n", e.message);
            break;
        } catch (IOChannelError e) {
            stderr.printf("io channel error: %s\n", e.message);
            break;
        }

        if (status == IOStatus.NORMAL) {
            if (! processor(sb.str))
                break;
            continue;
        } else if (status == IOStatus.AGAIN) {
            continue;
        } else {
            break;
        }
    }

    Process.close_pid(child_pid);
    return true;
}


private class NmOutputParser : Object {
    private Regex               _regex;
    private CSymbolProcessor    _proc;

    public NmOutputParser(CSymbolProcessor proc) {
        _proc = proc;
        try {
            _regex = new Regex("""^([0-9a-f]{8}) ([0-9a-f]{8}) . (.+?)(?:\s+(\/.+):(\d+))?$""",
                               RegexCompileFlags.OPTIMIZE, 0);
        } catch (RegexError e) {
            stderr.printf("Bad regex: %s\n", e.message);
        }
    }

    public bool parseLine(string s) {
        //stdout.printf("Nm output: %s", s);

        MatchInfo match_info;
        if (_regex.match(s, 0, out match_info)) {
            string addr_str  = match_info.fetch(1);
            string size_str  = match_info.fetch(2);
            string signature = match_info.fetch(3);
            string file      = match_info.fetch(4);
            string line_str  = match_info.fetch(5);

            uint addr = (uint)(addr_str.to_ulong(null, 16));
            uint size = (uint)(size_str.to_ulong(null, 16));
            uint line = (uint)(line_str == null ? 0
                                                : line_str.to_ulong(null, 16));
            //stdout.printf("matched: %s:%s [%s] %s:%s\n",
            //              addr_str, size_str, signature,
            //              file, line_str);
            _proc(addr, size, signature, file, line);
        }

        return true;
    }
}

public class Symbol : Object {
    /** code size   */
    public uint size { get; set; }

    /** signature   */
    public string signature { get; set; }

    /** file path where this function is defined    */
    public string? file { get; set; }

    /** line number in file */
    public uint line { get; set; }

    public Symbol(uint size, string signature,
                  string? file = null, uint line = 0) {
        _size = size;
        _signature = signature;
        _file = file;
        _line = line;
    }
}


public class Callee : Object {
    /** file path where this function is called  */
    public string? file { get; set; }

    /** line number in file where this function is called   */
    public uint line { get; set; }

    /** information about this function */
    public Symbol symbol { get; set; }

    public Callee(Symbol symbol, string? file = null, uint line = 0) {
        _symbol = symbol;
        _file = file;
        _line = line;
    }
}


public class CSymbol : Symbol {
    /** virtual address in object file  */
    public uint address { get; set; }

    public CSymbol(uint address, uint size, string signature,
                   string? file = null, uint line = 0) {
        base(size, signature, file, line);
        _address = address;
    }
}


public class Module : Object {
    /** file path of this module file   */
    public string file { get; set; }

    public Module(string file) {
        _file = file;
    }
}


public class CModule: Module {
    private HashTable<uint, CSymbol>        _symbols;
    private HashTable<uint, Array<Callee>>  _callees;

    private void addSymbol(uint address, uint size,
                           string signature,
                           string? file = null, uint line = 0) {
        //stdout.printf("addSymbol: %u %u %s %s:%u\n", address, size,
        //              signature, file, line);
        _symbols.insert(address,
                        new CSymbol(address, size, signature, file, line));
    }

    public CModule(string file) {
        base(file);
        _symbols = new HashTable<uint, CSymbol>(null, null);
        _callees = new HashTable<uint, Array<Callee>>(null, null);
    }

    public bool load() {
        NmOutputParser parser = new NmOutputParser(addSymbol);

        return CallView.pipe({"nm", "-C", "--defined-only", "-l",
                                    "-n", "-S", file, null},
                             null,
                             parser.parseLine);
    }
}


} // end namespace CallView

