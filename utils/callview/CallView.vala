using GLib;

namespace CallView {

public delegate bool LineProcessor(string line);

public static bool pipe(string[] argv, string[]? envp,
                        LineProcessor processor) {
    Pid child_pid;
    int output_fd;

    try {
        if (Process.spawn_async_with_pipes(null, argv, envp,
                                           SpawnFlags.SEARCH_PATH, null,
                                           out child_pid,
                                           null, out output_fd)) {
            //stderr.printf("pipe ok\n");

            IOChannel io = new IOChannel.unix_new(output_fd);
            StringBuilder sb = new StringBuilder();
            size_t terminator_pos;
            IOStatus status;

            for (;;) {
                status = io.read_line_string(sb, out terminator_pos);

                if (status == IOStatus.NORMAL) {
                    if (! processor(sb.str))
                        break;
                    continue;
                } else if (status == IOStatus.AGAIN) {
                    continue;
                }

                break;
            }

            Process.close_pid(child_pid);
        } else {
            //stderr.printf("pipe bad\n");
        }
    } catch (SpawnError e) {
        stderr.printf("pipe error: %s \n", e.message);
        return false;
    }

    return true;
}


public class Function : Object {
    /** code size   */
    public uint size { get; set; }

    /** signature   */
    public string signature { get; set; }

    /** file path where this function is defined    */
    public string? file { get; set; }

    /** line number in file */
    public uint line { get; set; }

    public Function(uint size, string signature,
                    string? file = null, uint line = 0) {
        _size = size;
        _signature = signature;
        _file = file;
        _line = line;
    }
}


public class CFunction : Function {
    /** virtual address in object file  */
    public uint address { get; set; }

    public CFunction(uint address, uint size, string signature,
                     string? file = null, uint line = 0) {
        base(size, signature, file, line);
        _address = address;
    }
}


public class ObjectFile : Object {
    /** file path of this object file   */
    public string file { get; set; }

    public ObjectFile(string file) {
        _file = file;
    }
}


public class CObjectFile: ObjectFile {
    private HashTable<uint, CFunction> _symbols;

    private bool NmOutputProcessor(string line) {
        stdout.printf("Nm output: %s", line);
        return true;
    }

    public CObjectFile(string file) {
        base(file);
        _symbols = new HashTable<uint, CFunction>(null, null);
    }

    public bool parse() {
        return CallView.pipe({"nm", "-C", "--defined-only", "-l",
                                    "-n", "-S", file, null},
                             null,
                             NmOutputProcessor);
    }
}


} // end namespace CallView

