package swf;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import static org.testng.Assert.*;
import org.testng.annotations.*;

import swf.decoder.FileHeader;


public final class FileHeaderTest {

    public byte[] readSWFFile(String filename) {
        File file;
        FileInputStream in;

        file = new File(filename);

        try {
            in = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            fail("Can't open " + filename + " to read", e);
            return null;
        }

        byte[] data;
        
        try {
            data = new byte[64];

            in.read(data, 0, 8);

            if ((byte)'C' == data[0]) {
                InflaterInputStream zip = new InflaterInputStream(in);
                zip.read(data, 8, 64 - 8);
                zip.close();
            } else {
                in.read(data, 8, 64 - 8);
            }
            in.close();
        } catch (IOException e) {
            fail("Can't read " + filename, e);
            return null;
        }

        return data;
    }

    @Test
    @Parameters({"filename", "expect"})
    public void checkSWFFileHeader(String filename, String expect) {
        byte[] data = readSWFFile(filename);
        if (null == data)
            return;

        ByteArrayReader bar = new ByteArrayReader(data, 0);
        FileHeader header = new FileHeader(bar);

        StringBuffer sb = new StringBuffer();

        if (header.mSignature[0] == (byte)'C') {
            sb.append("CWS, version=");
            assertEquals(header.mSignature, (new String("CWS")).getBytes(), "wrong header");
        } else if (header.mSignature[0] == (byte)'F') {
            sb.append("FWS, version=");
            assertEquals(header.mSignature, (new String("FWS")).getBytes(), "wrong header");
        } else {
            fail("wrong header");
        }

        sb.append(header.mVersion);
        sb.append(", fileLength=");
        sb.append(header.mFileLength);
        sb.append(", frameSize=");
        sb.append(header.mFrameWidth + " x " + header.mFrameHeight + ", frameRate=");
        sb.append(header.mFrameRate);
        sb.append(", frameCount=");
        sb.append(header.mFrameCount);

        System.out.println("got   : " + sb.toString());
        System.out.println("expect: " + expect);
        System.out.flush();

        assertEquals(sb.toString(), expect);
    }
}

