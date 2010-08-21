import java.io.*;

import com.wolfram.jlink.*;

public class Mathematica {
    private int inputCount;

    public static void main(String[] args) {
        Mathematica m = new Mathematica();
        m.run(args);
    }

    public String readLine(BufferedReader in) throws IOException {
        System.out.print("Input[" + inputCount++ + "] := ");

        return in.readLine();
    }

    public void run(String[] args) {
        String[] kl_args;
        String MATH_ROOT = System.getenv("MATH_ROOT");
        boolean isDebug = System.getenv("MATH_DEBUG") != null;

        if (MATH_ROOT == null) {
            kl_args = new String[] { "-linkmode", "launch", "-linkname", "math -mathlink" };
        } else {
            kl_args = new String[] { "-linkmode", "launch", "-linkname",
                "\"" + MATH_ROOT + "/Executables/math\" -mathlink" };
        }

        KernelLink kl;

        try {
            kl = MathLinkFactory.createKernelLink(kl_args);

            if (isDebug)
                kl.addPacketListener(new PacketPrinter(System.err));

            kl.addPacketListener(new MyPacketListener());
        } catch (MathLinkException e) {
            e.printStackTrace();
            return;
        }

        try {
            // Get rid of the initial InputNamePacket the kernel will send when it is launched.
            kl.discardAnswer();

            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String input, output;

            while (null != (input = readLine(in))) {
                output = kl.evaluateToOutputForm(input, 70);
                System.out.println(output);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (MathLinkException e) {
            e.printStackTrace();
        } finally {
            kl.close();
        }
    }

    class MyPacketListener implements PacketListener {
        public boolean packetArrived(PacketArrivedEvent evt) throws MathLinkException {
            if (evt.getPktType() == MathLink.TEXTPKT) {
                KernelLink kl = (KernelLink) evt.getSource();
                System.out.println(kl.getString());
            }

            return true;
        }
    }
}

