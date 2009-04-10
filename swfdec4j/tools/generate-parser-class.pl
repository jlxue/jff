while (<>) {
    chomp;
    open F, ">", $_. "Tag.java" || die "$_: $!\n";
    print F <<"EOF";
package swf.decoder;

import swf.ByteArrayReader;


public class ${_}Tag extends Tag {

    public void decodeBody(ByteArrayReader bar) {
    }
}

EOF
    close F;
}

