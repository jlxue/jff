package swf.decoder;

import swf.ByteArrayReader;


public final class FileHeader {

    public byte[]      mSignature;
    public byte        mVersion;
    public int         mFileLength;
    public int         mFrameWidth;
    public int         mFrameHeight;
    public double      mFrameRate;
    public short       mFrameCount;

    public int         mSize;

    public FileHeader(ByteArrayReader bar) {
        mSignature = new byte[3];
        int[] frameSize = new int[4];

        assert(3 == bar.readBytes(mSignature, 0, 3));

        mVersion = bar.readByte();
        mFileLength = bar.readInt();

        int bits = bar.readRect(frameSize);
        mFrameWidth = frameSize[1];
        mFrameHeight = frameSize[3];

        mFrameRate = bar.readShortFixed();
        mFrameCount = bar.readShort();

        mSize = 3 + 1 + 4 + (bits + 8) / 8 + 2 + 2;
    }

}

