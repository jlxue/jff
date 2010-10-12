#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;
use Term::ReadLine;
use Net::IMAP::Simple;

# gmail:
# imap.googlemail.com 993 ssl/tls
# smtp.googlemail.com 465 ssl/tls

my %commands = (
    find        => \&cmd_find,
    help        => \&cmd_help,
    list        => \&cmd_list,
    open        => \&cmd_open,
    status      => \&cmd_status,
);
my @command_names = sort keys %commands;

#### connect

my $user   = "";
my $passwd = "";
my $imap = Net::IMAP::Simple->new("imap.googlemail.com",
                                  port              => 993,
                                  use_ssl           => 1,
                                  use_select_cache  => 0,
                                  select_cache_ttl  => 120,
                                  debug             => 0);
die "Unable to connect to IMAP: $Net::IMAP::Simple::errstr\n" if ! $imap;

#### user and passwd

print "User: ";
chomp($user = <STDIN>);

print "Passwd: ";
ReadMode 2;
chomp($passwd = ReadLine(0));
ReadMode 0;
if (! $imap->login($user, $passwd)) {
    my $errstr = $imap->errstr;
    $imap->quit;
    die "Unable to login: $errstr\n";
}
print "\n";


#### command loop

my $term = Term::ReadLine->new('MyMail');
my $out = $term->OUT || \*STDOUT;
while (defined ($_ = $term->readline(prompt()))) {
    s/^\s+|\s+$//g;

    last if /(?:e|x|q|exit|quit)$/i;
    next if length($_) == 0;

    $term->addhistory($_);

    my ($cmd, $arg) = $_ =~ /^(\S+)\s*(.*)$/;
    my @cmds = grep { index($_, $cmd) == 0 } @command_names;
    if (@cmds == 0) {
        print STDERR "Unknown command!\n";
        next;
    } elsif (@cmds > 1) {
        print STDERR "Unambigous command: @cmds\n";
        next;
    }

    if (length($arg) > 0) {
        $commands{$cmds[0]}->($arg);
    } else {
        $commands{$cmds[0]}->();
    }
}


$imap->quit;
exit 0;


###############################################################
sub cmd_status {
    my @folders = sort $imap->mailboxes(@_);
    my $current = $imap->current_box;

    for my $folder (@folders) {
        $imap->select($folder);
        my ($unseen, $recent, $num_messages) = $imap->status;
        my @flags = $imap->flags($folder);

        printf "%-20s %5d %5d %5d", $folder, $unseen, $recent, $num_messages;
        print @flags > 0 ? " (@flags)\n" : "\n";
    }

    $imap->select($current) if $current ne $imap->current_box;
}


sub cmd_open {
    $imap->select($_[0]);
}


sub cmd_help {
}


sub cmd_list {
}

sub prompt {
    my $current_box = $imap->current_box;
    return "$current_box>";
}

