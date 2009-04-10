package swf;

public final class ByteArrayReader {

    private byte[] data;
    private int offset;

    private int bitBuf;
    private int bitPos;


    public ByteArrayReader(byte[] data, int offset) {
        // check null and array bound
        byte unused = data[offset];

        this.data = data;
        this.offset = offset;
    }


    public byte readByte() {
        return data[offset++];
    }


    public int readBytes(byte[] dest, int off, int len) {
        int avail = data.length - offset;

        if (len > avail)
            len = avail;

        System.arraycopy(data, offset, dest, off, len);

        offset += len;
        return len;
    }


    /**
     * Translate byte array in little endian to short.
     */
    public short readShort() {
        int r = (int)data[offset + 1] & 0xFF;
        r = (r << 8) | ((int)data[offset] & 0xFF);
        offset += 2;
        return (short)r;
    }


    public double readShortFixed() {
        return readShort() / 256.0;
    }


    /**
     * Translate byte array in little endian to int.
     */
    public int readInt() {
        int i = offset + 3;
        int r = data[i--];
        r = (r << 8) | ((int)data[i--] & 0xFF);
        r = (r << 8) | ((int)data[i--] & 0xFF);
        r = (r << 8) | ((int)data[i] & 0xFF);
        offset += 4;
        return r;
    }


    /**
     * Translate byte array in little endian to long.
     */
    public long readLong() {
        int i = offset + 7;
        long r = data[i--];
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i--] & 0xFF);
        r = (r << 8) | ((long)data[i] & 0xFF);
        offset += 8;
        return r;
    }


    /**
     * translate compact RECT in swf to an integer array.
     *
     * @return
     *      total number of bits read
     */
    public int readRect(int[] rect) {
        int bits = (int)readUBits(5);

        for (int i = 0; i < 4; ++i) {
            rect[i] = (int)readSBits(bits);
        }

        bitPos = 0;     // skip padding bits
        return 5 + 4 * bits;
    }


    /**
     * Read an unsigned value from the given number of bits.
     *
     *  @param numBits         number of bits to be read
     *
     *  @return the unsigned value read from the bit stream
     *
     * Modified from javaswf/src/com/anotherbigidea/io/InStream.java
     */
    public long readUBits(int numBits)
    {
        if (numBits == 0)
            return 0;

        long result = 0;

        if (bitPos == 0)    {   // no value in the buffer, read a byte
            bitBuf = (int)data[offset++] & 0xFF;
            bitPos = 8;
        }

        while (true) {
            int shift = numBits - bitPos;
            if(shift > 0) {
                // Consume the entire buffer
                result |= bitBuf << shift;
                numBits -= bitPos;

                // Get the next byte from the input buffer
                bitBuf = (int)data[offset++] & 0xFF;
                bitPos = 8;
            } else {
                // Consume a portion of the buffer
                result |= bitBuf >> -shift;
                bitPos -= numBits;
                bitBuf &= 0xFF >> (8 - bitPos); // mask off the consumed bits

                return result;
            }
        }
    }


    /**
     * Read a signed value from the given number of bits.
     *
     * Modified from javaswf/src/com/anotherbigidea/io/InStream.java
     */
    public long readSBits(int numBits)
    {
        // Get the number as an unsigned value.
        long uBits = readUBits(numBits);

        // Is the number negative?
        if ((uBits & (1L << (numBits - 1))) != 0)
        {
            // Yes. Extend the sign.
            uBits |= -1L << numBits;
        }

        return uBits;
    }


    public void skipPaddingBits() {
        bitPos = 0;
    }


    public int getOffset() {
        return offset;
    }


    public void setOffset(int offset) {
        this.offset = offset;
    }
}

