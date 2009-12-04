#!/usr/bin/perl
use Storable qw/lock_store lock_retrieve/;

use strict;
use warnings;

use constant {
    IDX_SIZE    => 0,       # size
    IDX_TYPE    => 1,       # type
    IDX_SIGN    => 2,       # signature
    IDX_PATH    => 3,       # file path
    IDX_LINE    => 4,       # line number
    IDX_CALL    => 5,       # addresses of callees
};


my ($addrinfo, $syminfo) = retrieve_symbols('addrinfo', 'syminfo', $ARGV[0]);

my @addrs;

print "> ";
while (<STDIN>) {
    chomp;

    if (/\d+/) {
        if ($_ < 1 || $_ > @addrs) {
            print "Bad choice!\n> ";
            next;
        }
        my $callees = retrieve_callee_info($addrinfo, $addrs[$_ - 1], $ARGV[0]);
        @addrs = ();
        for (my $i = 1; $i < @$callees; $i +=2) {
            push @addrs, $callees->[$i];
        }
    } else {
        @addrs = ();
        while (my ($k, $v) = each %$syminfo) {
            push @addrs, @$v if $k =~ /$_/i;
        }

        if (@addrs == 0) {
            print "Bad symbol!\n> ";
            next;
        }
    }

    my $i = 1;
    for (@addrs) {
        printf "%2d: $_ ", $i++;
        if (exists $addrinfo->{$_}) {
            print $addrinfo->{$_}[IDX_PATH], ":",
                  $addrinfo->{$_}[IDX_LINE], " \t",
                  $addrinfo->{$_}[IDX_SIGN], "\n";
        } else {
            print "# external function\n";
        }
    }

    print "> ";
}


##################################################################
# extract_symbols($program)
#
# returns: ($addrinfo, $syminfo)
#   $addrinfo = {
#           addr1  => [size, type, signature, path, line, [calles_addr]],
#           ...
#           };
#   $syminfo = {
#           name   => [addr1, addr2],
#           };
sub extract_symbols {
    my ($program) = @_;
    my ($addrinfo, $syminfo) = ({}, {});

    open my $fh, "nm -C --defined-only -l -n -S $program |" or die;

    while (<$fh>) {
        chomp;

        my ($addr, $size, $type, $signature, $path, $line) =
            $_ =~ /^([0-9a-f]{8}) ([0-9a-f]{8}) (.) (.*?)(?:\s+(\/.*):(\d+))?$/;

        next if ! defined $signature;

        #print "$addr | $size | $type | $signature | $path:$line\n";

        $addrinfo->{$addr} = [hex($size), $type, $signature, $path, $line];

        if (exists $syminfo->{$signature}) {
            push @{$syminfo->{$signature}}, $addr;
        } else {
            $syminfo->{$signature} = [$addr];
        }
    }

    close $fh;

    return ($addrinfo, $syminfo);
}


sub retrieve_symbols {
    my ($addrfile, $symfile, $program) = @_;

    my $addrinfo = -f $addrfile ? lock_retrieve($addrfile) : undef;
    my $syminfo = -f $symfile ? lock_retrieve($symfile) : undef;

    if (! defined $addrinfo || ! defined $syminfo) {
        ($addrinfo, $syminfo) = extract_symbols($program);
        lock_store($addrinfo, $addrfile);
        lock_store($syminfo, $symfile);
    }

    return ($addrinfo, $syminfo);
}


sub extract_callee_info {
    my ($addrinfo, $addr, $program) = @_;
    my $callees = [];

    my $start = "0x$addr";
    my $stop  = sprintf "0x%08x", hex($addr) + $addrinfo->{$addr}[IDX_SIZE];

    open my $fh, "objdump -d -C -F -l -w --start-address $start " .
                 "--stop-address $stop $program |" or die;

    my $line = $addrinfo->{$addr}[IDX_LINE];
    my $callee;
    while (<$fh>) {
        if (/^\/.*:(\d+)/) {
            $line = $1;
        } elsif (/^ [0-9a-f]{7}:\t(?:[0-9a-f]{2}\s)+\s*call\s+([0-9a-f]+)/) {
            $callee = ('0' x (8 - length($1))) . $1;
            push @$callees, $line, $callee;
        }
    }

    close $fh;

    return $callees;
}


sub retrieve_callee_info {
    my ($addrinfo, $addr, $program) = @_;
    my $callees = $addrinfo->{$addr}[IDX_CALL];

    if (! defined $callees) {
        $callees = extract_callee_info($addrinfo, $addr, $program);
        $addrinfo->{$addr}[IDX_CALL] = $callees;
    }

    return $callees;
}

