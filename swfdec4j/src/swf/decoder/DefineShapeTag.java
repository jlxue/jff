package swf.decoder;

import swf.ByteArrayReader;


public class DefineShapeTag extends Tag {

    int         mShapeId;
    int[]       mShapeBounds;

    public void decodeBody(ByteArrayReader bar) {
        mShapeId = bar.readShort() & 0xFFFF;
    }
}

