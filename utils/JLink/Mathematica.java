import java.io.*;

import com.wolfram.jlink.*;

public class Mathematica {
    private int inputCount;

    public static void main(String[] args) {
        Mathematica m = new Mathematica();
        m.run(args);
    }

    public String readLine(BufferedReader in) throws IOException {
        System.out.print("In[" + ++inputCount + "]:= ");

        return in.readLine();
    }

    public void run(String[] args) {
        String[] kl_args;
        String MATH_ROOT = System.getenv("MATH_ROOT");
        boolean isDebug = System.getenv("MATH_DEBUG") != null;
        int windowWidth = 0;
        if (System.getenv("MATH_WINDOW_WIDTH") != null) {
            try {
                windowWidth = Integer.parseInt(System.getenv("MATH_WINDOW_WIDTH"));
            } catch (Exception e) {
                e.printStackTrace();
            }

            if (windowWidth < 0)
                windowWidth = 0;
        }

        if (MATH_ROOT == null) {
            kl_args = new String[] { "-linkmode", "launch", "-linkname", "math -mathlink" };
        } else {
            kl_args = new String[] { "-linkmode", "launch", "-linkname",
                "\"" + MATH_ROOT + "/Executables/math\" -mathlink" };
        }

        KernelLink kl = null;

        try {
            kl = MathLinkFactory.createKernelLink(kl_args);

            if (isDebug)
                kl.addPacketListener(new PacketPrinter(System.err));

            kl.addPacketListener(new MyPacketListener());

            kl.connect(10000);
        } catch (MathLinkException e) {
            e.printStackTrace();
            if (kl != null)
                kl.close();
            return;
        }

        try {
            // Get rid of the initial InputNamePacket the kernel will send when it is launched.
            kl.discardAnswer();

            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String input, output;

            while (null != (input = readLine(in))) {
                // Method 1:
                output = kl.evaluateToOutputForm(input, windowWidth);
                System.out.println(output);


                /*
                // Method 2:
                kl.evaluate(input);
                kl.waitForAnswer();

                Expr expr = kl.getExpr();
                System.out.println(expr);
                expr.dispose();
                */


                /*
                // Method 3:
                kl.putFunction("EnterTextPacket", 1);
                kl.put(input);

                //kl.putFunction("EnterExpressionPacket", 1);
                //kl.putFunction("ToString", 1);
                //kl.putFunction("ToExpression", 1);
                //kl.put(input);

                kl.discardAnswer();     // let PacketListener process all packets
                */
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
            int type = evt.getPktType();
            KernelLink kl = (KernelLink) evt.getSource();

            //System.out.println("got pkt type: " + type);
            //System.out.println("evt: " + evt);

            switch (type) {
                case MathLink.RETURNEXPRPKT:
                //case MathLink.RETURNPKT:
                case MathLink.RETURNTEXTPKT:
                case MathLink.TEXTPKT:
                    System.out.println(kl.getString());
                    break;

                //case MathLink.INPUTNAMEPKT:
                case MathLink.OUTPUTNAMEPKT:
                    System.out.print(kl.getString());
                    break;

                default:
                    //System.out.println("default got pkt type: " + type);
                    break;
            }

            return true;
        }
    }
}

