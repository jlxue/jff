#!/usr/bin/perl
use strict;
use warnings;
use Digest::MD5 qw/md5 md5_hex/;

# See http://hg.dovecot.org/dovecot-2.0/file/38972af8bd29/src/auth/mech-digest-md5.c

# "username:realm:password" => 16 bytes MD5 digest
my $credential = md5('dieken*webmail:corp.example.com:123456');

my $nonce = $ARGV[0] || "random-string-generated-by-server";
my $cnonce = $ARGV[1] || "random-string-generated-by-client";
my $nonce_count = $ARGV[2] || 1;    # monotone increasing serial number
my $authorization_id = "";
my $qop = "auth";   # or auth-int, auth-conf  (quality of protocol)
my $digest_uri = "imap/imap.corp.example.com";
my ($a1_hex, $a2_hex);

if ($authorization_id) {
    $a1_hex = md5_hex("$credential:$nonce:$cnonce:$authorization_id");
} else {
    $a1_hex = md5_hex("$credential:$nonce:$cnonce");
}

if ($qop eq 'auth-int' or $qop eq 'auth-conf') {
    # most servers and clients use dummy MD5 digest for body
    $a2_hex = md5_hex("AUTHENTICATE:$digest_uri:" . ('0' x 32));
} else {
    $a2_hex = md5_hex("AUTHENTICATE:$digest_uri");
}

print "a1_hex=$a1_hex, a2_hex=$a2_hex, response=",
    md5_hex("$a1_hex:$nonce:" .
            sprintf("%08ld", $nonce_count) .
            ":$cnonce:$qop:$a2_hex"), "\n";

