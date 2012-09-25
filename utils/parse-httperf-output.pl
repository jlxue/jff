#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;

my $prefix = "";
my $suffix = "";
my @headers =qw(
    connections requests replies test-duration
    conn/s ms/conn concurrent-connections
    connection-lifecycle-min connection-lifecycle-avg connection-lifecycle-max connection-lifecycle-median connection-lifecycle-stddev
    connection-time
    replies/conn
    req/s ms/req
    request-size
    replies/s-min replies/s-avg replies/s-max replies/s-stddev samples
    reply-time-response reply-time-transfer
    reply-size-header reply-size-content reply-size-footer reply-size-total
    reply-status-1xx reply-status-2xx reply-status-3xx reply-status-4xx reply-status-5xx
    CPU-time-user CPU-time-system CPU-time-user-% CPU-time-system-% CPU-time-total-%
    KB/s
    errors-total errors-client-timo errors-socket-timo errors-connrefused errors-connrest
    errors-fd-unavail errors-addr-unavail errors-ftab-full errors-other);

GetOptions("prefix=s" => \$prefix, "suffix=s" => \$suffix);
@headers = map { "$prefix$_$suffix" } @headers;
print "@headers\n";


while (<>) {
    if (/^Total: connections (\d+) requests (\d+) replies (\d+) test-duration ([\d\.]+) s/) {
        print "$1 $2 $3 $4 ";
    }

    if (/^Connection rate: ([\d\.]+) conn\/s \(([\d\.]+) ms\/conn, <=(\d+)/) {
        print "$1 $2 $3 ";
    }

    if (/^Connection time \[ms\]: min ([\d\.]+) avg ([\d\.]+) max ([\d\.]+) median ([\d\.]+) stddev ([\d\.]+)/) {
        print "$1 $2 $3 $4 $5 ";
    }

    if (/^Connection time \[ms\]: connect ([\d\.]+)/) {
        print "$1 ";
    }

    if (/^Connection length \[replies\/conn\]: ([\d\.]+)/) {
        print "$1 ";
    }

    if (/^Request rate: ([\d\.]+) req\/s \(([\d\.]+) ms\/req\)/) {
        print "$1 $2 ";
    }

    if (/^Request size \[B\]: ([\d\.]+)/) {
        print "$1 ";
    }

    if (/^Reply rate \[replies\/s\]: min ([\d\.]+) avg ([\d\.]+) max ([\d\.]+) stddev ([\d\.]+) \(([\d\.]+) samples\)/) {
        print "$1 $2 $3 $4 $5 ";
    }

    if (/^Reply time \[ms\]: response ([\d\.]+) transfer ([\d\.]+)/) {
        print "$1 $2 ";
    }

    if (/^Reply size \[B\]: header ([\d\.]+) content ([\d\.]+) footer ([\d\.]+) \(total ([\d\.]+)\)/) {
        print "$1 $2 $3 $4 ";
    }

    if (/^Reply status: 1xx=([\d\.]+) 2xx=([\d\.]+) 3xx=([\d\.]+) 4xx=([\d\.]+) 5xx=([\d\.]+)/) {
        print "$1 $2 $3 $4 $5 ";
    }

    if (/^CPU time \[s\]: user ([\d\.]+) system ([\d\.]+) \(user ([\d\.]+)% system ([\d\.]+)% total ([\d\.]+)%\)/) {
        print "$1 $2 $3 $4 $5 ";
    }

    if (/^Net I\/O: ([\d\.]+) KB\/s/) {
        print "$1 ";
    }

    if (/^Errors: total ([\d\.]+) client-timo ([\d\.]+) socket-timo ([\d\.]+) connrefused ([\d\.]+) connreset ([\d\.]+)/) {
        print "$1 $2 $3 $4 $5 ";
    }

    if (/^Errors: fd-unavail ([\d\.]+) addrunavail ([\d\.]+) ftab-full ([\d\.]+) other ([\d\.]+)/) {
        print "$1 $2 $3 $4\n";
    }
}
