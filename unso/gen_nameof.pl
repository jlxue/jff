#!/usr/bin/perl -w
use strict;
use warnings;
use diagnostics;

my $name;
my @defines;
my @values;

open FH, "< /usr/include/elf.h" || die "Can't open elf.h: $!\n";
@defines = <FH>;
close FH;

@defines = grep /^#\s*define\s+/, @defines;

print "/* -------This file is generated, don't edit it ---------- */\n";
print "#include <elf.h>\n\n";

while (<STDIN>) {
    ($name) = $_ =~ /char\* nameof(.*)\(.*/;
    s/;\s*$//;
    print $_, " {\n";
    print "    switch (param) {\n";


    @values = grep /^#\s*define\s+$name/, @defines;
    @values = uniq(@values);
    foreach my $v (@values) {
        print "    case $v:\n        return \"$v\";\n";
    }
    
    print "    default:\n        return \"--other--\";\n";
    print "    }\n}\n\n";
}

sub uniq {
    my @values = ();
    my ($name, $val);
    my %hash;

    for (my $i = 0; $i < @_; ++$i) {
        ($name, $val) = $_[$i] =~ /define\s+([^\s]+)\s+([^\s]+)/;
        next if $val =~ /[A-Z]/;
        if (! exists $hash{$val}) {
            $hash{$val} = $name;
            push @values, $name;
        }
    }

    return @values;
}

