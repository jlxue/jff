using GLib;

namespace CallView {

public delegate bool LineProcessor(string line);

public delegate void CSymbolProcessor(uint address, uint size,
                                      string signature,
                                      string? file, uint line);

public delegate void CCalleeProcessor(Array<Callee> callees,
                                      uint callee_addr,
                                      string? callee_signature,
                                      string? file,
                                      uint line);


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


private class ObjdumpOutputParser : Object {
    private Regex   _regex1;
    private Regex   _regex2;
    private CSymbol _caller;
    private unowned Array<Callee> _callees;
    private CCalleeProcessor _proc;
    private string _file;
    private uint _line;

    public ObjdumpOutputParser(CSymbol caller, Array<Callee> callees,
                               CCalleeProcessor proc) {
        _caller = caller;
        _callees = callees;
        _proc = proc;
        _file = null;
        _line = 0;

        try {
            _regex1 = new Regex("""^(\/.+):(\d+)$""",
                                RegexCompileFlags.OPTIMIZE, 0);
            _regex2 = new Regex(""" [0-9a-f]{7}:\t(?:[0-9a-f]{2}\s)+\s*call\s+([0-9a-f]+)(?:\s+<(.+)>)?""",
                                RegexCompileFlags.OPTIMIZE, 0);
        } catch (RegexError e) {
            stderr.printf("Bad regex: %s\n", e.message);
        }
    }

    public bool parseLine(string s) {
        stdout.printf("Objdump output: %s", s);

        MatchInfo match_info;
        if (_regex1.match(s, 0, out match_info)) {
            _file = match_info.fetch(1);
            _line = (uint)(match_info.fetch(2).to_ulong());
        } else if (_regex2.match(s, 0, out match_info)) {
            uint  callee_addr = (uint)(match_info.fetch(1).to_ulong(null, 16));
            string? callee_signature = match_info.fetch(2);

            if (_caller.file == _file) {
                _proc(_callees, callee_addr, callee_signature, null, _line);
            } else {
                _proc(_callees, callee_addr, callee_signature, _file, _line);
            }
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

    private void addCallee(Array<Callee> callees,
                           uint callee_address,
                           string? signature = null,
                           string? file = null, uint line = 0) {
        Symbol? symbol = _symbols.lookup(callee_address);
        if (null == symbol) {
            symbol = new CSymbol(0, 0, signature, "-* external *-", 0);
        }

        Callee callee = new Callee(symbol, file, line);
        callee.ref();      // XXX: why can't append_val((owned)callee) ?
        callees.append_val(callee);
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

    public unowned Array<Callee>? queryCallees(Symbol symbol) {
        if (! (symbol is CSymbol)) {
            return null;
        }

        CSymbol caller = (CSymbol)symbol;
        unowned Array<Callee>? callees = _callees.lookup(caller.address);
        if (null != callees) {
            return callees;
        }

        _callees.insert(caller.address, new Array<Callee>(false, false,
                                                          (uint)sizeof(Callee)));
        callees = _callees.lookup(caller.address);

        ObjdumpOutputParser parser = new ObjdumpOutputParser(caller,
                                                             callees,
                                                             addCallee);

        if (CallView.pipe({"objdump", "-d", "-C", "-F", "-l", "-w",
                           "--start-address",
                           caller.address.to_string(),
                           "--stop-address",
                           (caller.address + caller.size).to_string(), null},
                          null,
                          parser.parseLine)) {
            return callees;
        } else {
            return null;
        }
    }
}


} // end namespace CallView

