import java.io.*;

import com.wolfram.jlink.*;

public class Mathematica {
    public static String readLine(BufferedReader in) throws IOException {
        System.out.print("> ");

        return in.readLine();
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            args = new String[] { "-linkmode", "launch", "-linkname", "math -mathlink"};
        }

        KernelLink kl;

        try {
            kl = MathLinkFactory.createKernelLink(args);
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
                output = kl.evaluateToOutputForm(input, 0);
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
}

