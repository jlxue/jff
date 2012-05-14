#!/bin/sh

/usr/bin/find /tmp -maxdepth 2 -name 'agent.*' -user "$USER" 2>/dev/null |
    perl -ne '
        use File::Path qw/remove_tree/;
        use IO::Socket::UNIX;
        use strict;
        use warnings;

        chomp;
        if ( -S $_ && /^([^\/]+)\/agent\.(\d+)$/ ) {
            my $s = IO::Socket::UNIX->new($_);
            if(defined $s) {
                print STDERR "Found valid ssh-agent-sock file: $_\n";
                $s->close;
            } else {
                remove_tree($1, { verbose => 1 });
            }
        }'

