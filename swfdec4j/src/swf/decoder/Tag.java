package swf.decoder;

import swf.ByteArrayReader;

public class Tag {

    public short   mCode;
    public int     mLength;


    public void setCodeAndLength(short code, int length) {
        mCode = code;
        mLength = length;
    }


    public void decodeHeader(ByteArrayReader bar) {
        mCode = bar.readShort();
        mLength = mCode & 0x3F;
        mCode &= ~0x3F;

        if (mLength == 0x3F)
            mLength = bar.readInt();

        assert(mCode >= End && mCode < mConcreteTags.length && mLength >= 0);
    }


    public void decodeBody(ByteArrayReader bar) {
        // just skip the body
        bar.setOffset(bar.getOffset() + mLength);
    }


    public static Tag getUnknownTag() {
        return mUnknownTag;
    }


    public static Tag getConcreteTag(int code) {
        if (code >= mConcreteTags.length)
            return mUnknownTag;
        return mConcreteTags[code];
    }


    public static final short End                          = 0;
    public static final short ShowFrame                    = 1;
    public static final short DefineShape                  = 2;
    public static final short Unknown_3                    = 3;
    public static final short PlaceObject                  = 4;
    public static final short RemoveObject                 = 5;
    public static final short DefineBits                   = 6;
    public static final short DefineButton                 = 7;
    public static final short JPEGTables                   = 8;
    public static final short SetBackgroundColor           = 9;
    public static final short DefineFont                   = 10;
    public static final short DefineText                   = 11;
    public static final short DoAction                     = 12;
    public static final short DefineFontInfo               = 13;
    public static final short DefineSound                  = 14;
    public static final short StartSound                   = 15;
    public static final short Unknown_16                   = 16;
    public static final short DefineButtonSound            = 17;
    public static final short SoundStreamHead              = 18;
    public static final short SoundStreamBlock             = 19;
    public static final short DefineBitsLossless           = 20;
    public static final short DefineBitsJPEG2              = 21;
    public static final short DefineShape2                 = 22;
    public static final short DefineButtonCxform           = 23;
    public static final short Protect                      = 24;
    public static final short Unknown_25                   = 25;
    public static final short PlaceObject2                 = 26;
    public static final short Unknown_27                   = 27;
    public static final short RemoveObject2                = 28;
    public static final short Unknown_29                   = 29;
    public static final short Unknown_30                   = 30;
    public static final short Unknown_31                   = 31;
    public static final short DefineShape3                 = 32;
    public static final short DefineText2                  = 33;
    public static final short DefineButton2                = 34;
    public static final short DefineBitsJPEG3              = 35;
    public static final short DefineBitsLossless2          = 36;
    public static final short DefineEditText               = 37;
    public static final short Unknown_38                   = 38;
    public static final short DefineSprite                 = 39;
    public static final short Unknown_39                   = 40;
    public static final short Unknown_40                   = 41;
    public static final short Unknown_41                   = 42;
    public static final short FrameLabel                   = 43;
    public static final short Unknown_43                   = 44;
    public static final short SoundStreamHead2             = 45;
    public static final short DefineMorphShape             = 46;
    public static final short Unknown_47                   = 47;
    public static final short DefineFont2                  = 48;
    public static final short Unknown_49                   = 49;
    public static final short Unknown_50                   = 50;
    public static final short Unknown_51                   = 51;
    public static final short Unknown_52                   = 52;
    public static final short Unknown_53                   = 53;
    public static final short Unknown_54                   = 54;
    public static final short Unknown_55                   = 55;
    public static final short ExportAssets                 = 56;
    public static final short ImportAssets                 = 57;
    public static final short EnableDebugger               = 58;
    public static final short DoInitAction                 = 59;
    public static final short DefineVideoStream            = 60;
    public static final short VideoFrame                   = 61;
    public static final short DefineFontInfo2              = 62;
    public static final short Unknown_63                   = 63;
    public static final short EnableDebugger2              = 64;
    public static final short ScriptLimits                 = 65;
    public static final short SetTabIndex                  = 66;
    public static final short Unknown_67                   = 67;
    public static final short Unknown_68                   = 68;
    public static final short FileAttributes               = 69;
    public static final short PlaceObject3                 = 70;
    public static final short ImportAssets2                = 71;
    public static final short Unknown_72                   = 72;
    public static final short DefineFontAlignZones         = 73;
    public static final short CSMTextSettings              = 74;
    public static final short DefineFont3                  = 75;
    public static final short SymbolClass                  = 76;
    public static final short Metadata                     = 77;
    public static final short DefineScalingGrid            = 78;
    public static final short Unknown_79                   = 79;
    public static final short Unknown_80                   = 80;
    public static final short Unknown_81                   = 81;
    public static final short DoABC                        = 82;
    public static final short DefineShape4                 = 83;
    public static final short DefineMorphShape2            = 84;
    public static final short Unknown_85                   = 85;
    public static final short DefineSceneAndFrameLabelData = 86;
    public static final short DefineBinaryData             = 87;
    public static final short DefineFontName               = 88;
    public static final short StartSound2                  = 89;

    private static final UnknownTag mUnknownTag = new UnknownTag();
    private static final Tag[] mConcreteTags = new Tag[] {
        new EndTag(),
        new ShowFrameTag(),
        new DefineShapeTag(),
        mUnknownTag,
        new PlaceObjectTag(),
        new RemoveObjectTag(),
        new DefineBitsTag(),
        new DefineButtonTag(),
        new JPEGTablesTag(),
        new SetBackgroundColorTag(),
        new DefineFontTag(),
        new DefineTextTag(),
        new DoActionTag(),
        new DefineFontInfoTag(),
        new DefineSoundTag(),
        new StartSoundTag(),
        mUnknownTag,
        new DefineButtonSoundTag(),
        new SoundStreamHeadTag(),
        new SoundStreamBlockTag(),
        new DefineBitsLosslessTag(),
        new DefineBitsJPEG2Tag(),
        new DefineShape2Tag(),
        new DefineButtonCxformTag(),
        new ProtectTag(),
        mUnknownTag,
        new PlaceObject2Tag(),
        mUnknownTag,
        new RemoveObject2Tag(),
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        new DefineShape3Tag(),
        new DefineText2Tag(),
        new DefineButton2Tag(),
        new DefineBitsJPEG3Tag(),
        new DefineBitsLossless2Tag(),
        new DefineEditTextTag(),
        mUnknownTag,
        new DefineSpriteTag(),
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        new FrameLabelTag(),
        mUnknownTag,
        new SoundStreamHead2Tag(),
        new DefineMorphShapeTag(),
        mUnknownTag,
        new DefineFont2Tag(),
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        new ExportAssetsTag(),
        new ImportAssetsTag(),
        new EnableDebuggerTag(),
        new DoInitActionTag(),
        new DefineVideoStreamTag(),
        new VideoFrameTag(),
        new DefineFontInfo2Tag(),
        mUnknownTag,
        new EnableDebugger2Tag(),
        new ScriptLimitsTag(),
        new SetTabIndexTag(),
        mUnknownTag,
        mUnknownTag,
        new FileAttributesTag(),
        new PlaceObject3Tag(),
        new ImportAssets2Tag(),
        mUnknownTag,
        new DefineFontAlignZonesTag(),
        new CSMTextSettingsTag(),
        new DefineFont3Tag(),
        new SymbolClassTag(),
        new MetadataTag(),
        new DefineScalingGridTag(),
        mUnknownTag,
        mUnknownTag,
        mUnknownTag,
        new DoABCTag(),
        new DefineShape4Tag(),
        new DefineMorphShape2Tag(),
        mUnknownTag,
        new DefineSceneAndFrameLabelDataTag(),
        new DefineBinaryDataTag(),
        new DefineFontNameTag(),
        new StartSound2Tag()
    };
}

