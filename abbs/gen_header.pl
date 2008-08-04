#!/usr/bin/perl -w
use strict;
use warnings;

my $file = shift;
die "missing header file name!\n" if !defined $file;
die "$file exists!\n" if -e $file;

open F, ">$file" || die "Can't open $file: $!\n";
$file = uc $file;
$file =~ s/[^a-zA-Z0-9]/_/;
$file .= '__';
print F <<EOF;
#ifndef $file
#define $file

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __cplusplus
}
#endif

#endif /* $file */

EOF
close F;


