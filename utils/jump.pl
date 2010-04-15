#!/usr/bin/perl -w
#
# Run `perldoc ./jump.pl` for document.
#
# Author:
#   Liu Yubao <yubao.liu at gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2010-04-15  Liu Yubao
#       * initial release, v0.1

use strict;
use warnings;

use Cwd;
use Fcntl qw/:DEFAULT :flock/;
use File::Spec;
use Getopt::Long;
use Pod::Usage;
#use Smart::Comments '###';
use Storable qw/store_fd fd_retrieve lock_retrieve/;
use Term::ANSIColor;

use Class::Struct HistoryItem =>
    [
    access_time   => '$',
    hit_count     => '$',
    from_dirs     => '%',
    ];

use constant    {
    HISTORY_FILE    => ".jump_history",
    MODIFIED        => 0,
    REMOVED         => 1,
};

######################################################################
## global variables
my $g_opt_help = 0;
my $g_opt_man = 0;
my $g_opt_version = 0;
my $g_history_file = File::Spec->catfile($ENV{'HOME'}, HISTORY_FILE);
my $g_history;              # hash of working directories to HistoryItems
my @g_history_changed;      # changed entries to $g_history
my $g_opt_from_work_dir;    # old working directory
my $g_opt_action = 'search';# prune, dump, add, search
my %g_action_handler = (
    'add'       => \&action_add,
    'search'    => \&action_search,
    'dump'      => \&action_dump,
    'prune'     => \&action_prune,
);

######################################################################
## parse options
GetOptions("from=s"    => \$g_opt_from_work_dir,
           "action=s"  => \$g_opt_action,
           "help|?"    => \$g_opt_help,
           "man"       => \$g_opt_man,
           "version"   => \$g_opt_version,
          );
pod2usage(1) if $g_opt_help;
pod2usage(-exitstatus => 0, -verbose => 2) if $g_opt_man;
if ($g_opt_version) {
    print "$0 v0.1, by Liu Yubao <yubao.liu at gmail.com>\n";
    exit 0;
}

if (defined $g_opt_from_work_dir) {
    $g_opt_from_work_dir = File::Spec->rel2abs(File::Spec->canonpath($g_opt_from_work_dir));
} else {
    $g_opt_from_work_dir = getcwd() if !defined $g_opt_from_work_dir;
}

######################################################################
## load history file
$g_history = lock_retrieve($g_history_file) if -s $g_history_file;
$g_history = {} if !defined $g_history;

######################################################################
## handle action
if (exists $g_action_handler{$g_opt_action}) {
    my $ret = $g_action_handler{$g_opt_action}->(@ARGV);

    if (! $ret and @g_history_changed > 0) {
        $ret = safe_store_history();
    }

    exit $ret;
} else {
    print STDERR "ERROR: unknown action: $g_opt_action\n";
}

exit 1;

######################################################################
## subroutines
sub action_add {
    my $to_work_dir = shift;

    return 1 if !defined $to_work_dir or !-d $to_work_dir or !-d $g_opt_from_work_dir;
    $to_work_dir = File::Spec->rel2abs(File::Spec->canonpath($to_work_dir));

    return 0 if $g_opt_from_work_dir eq $to_work_dir;

    my $history_item;

    if (exists $g_history->{$to_work_dir}) {
        $history_item = $g_history->{$to_work_dir};
        $history_item->hit_count($history_item->hit_count + 1);
    } else {
        $history_item = new HistoryItem();
        $history_item->hit_count(1);
        $g_history->{$to_work_dir} = $history_item;
    }

    $history_item->access_time(time());

    my $dirs = $history_item->from_dirs;
    $dirs->{$g_opt_from_work_dir}++;

    $g_history_changed[MODIFIED] = [$to_work_dir, $g_opt_from_work_dir];

    return 0;
}

sub action_search {
    my $pattern = shift;
    my @candidates;
    my $choice;

    if (defined $pattern) {
        @candidates = grep /$pattern/, keys %$g_history;
    } else {
        @candidates = keys %$g_history;
    }

    return 1 if @candidates == 0;

    if (@candidates == 1) {
        $choice = $candidates[0];
    } else {
        if (-t STDOUT) {
            $choice = choice_directory(@candidates);
            return 1 if !defined $choice;
        } else {
            @candidates = sort_candidates(@candidates);
            for my $c (@candidates) {
                print $c, "\n";
            }

            return 0;
        }
    }

    print $choice, "\n";

    return 0;
}

sub action_prune {
    return 1;
}

sub action_dump {
    require Data::Dumper;
    Data::Dumper->import();

    print Dumper($g_history);
    return 0;
}

sub sort_candidates {
    return sort @_;
}

sub choice_directory {
    my @candidates = sort_candidates @_;
    my $choice;

    do {
        open my $pipe, "|less -FRX" or die "Can't pipe to \"less\": $!\n";
        for (my $i = 0; $i <= $#candidates; ++$i) {
            print $i + 1, "\t", $candidates[$i], "\n";
        }
        close $pipe;

        print "Which directory? [0 to quit, 1 ~ ", scalar(@candidates), " to select, or view again] ";
        $choice = <STDIN>;
        chomp $choice;
        $choice =~ s/^\s+//;

        return undef if $choice eq '0';
    } while ($choice <= 0 || $choice > @candidates);

    return $candidates[$choice - 1];
}

sub safe_store_history {
    sysopen my $fh, $g_history_file, O_RDWR | O_CREAT or die "Can't write to $g_history_file: $!\n";
    flock $fh, LOCK_EX or die "Can't lock $g_history_file: $!\n";

    my $history = fd_retrieve($fh) if -s $fh;
    $history = {} if !defined $history;

    my $h = $g_history_changed[MODIFIED];
    if (defined $h) {
        my ($to, $from) = @$h;
        my $ondisk_item = $history->{$to};
        my $item = $g_history->{$to};

        if (defined $ondisk_item) {
            $ondisk_item->access_time($item->access_time) if
                    $ondisk_item->access_time < $item->access_time;
            $ondisk_item->hit_count($ondisk_item->hit_count + 1);

            my $dirs = $ondisk_item->from_dirs;
            $dirs->{$from}++;
        } else {
            $history->{$to} = $g_history->{$to};
        }
    }

    $h = $g_history_changed[REMOVED];
    if (defined $h) {
    }

    sysseek($fh, 0, 0) or die "Can't seek in $g_history_file: $!\n";
    truncate($fh, 0) or die "Can't truncate $g_history_file: $!\n";
    my $ret = store_fd($history, $fh);
    $ret = $ret ? 0 : 1;

    flock $fh, LOCK_UN;
    close $fh;

    return $ret;
}

__END__

=head1 NAME

jump.pl - auxiliary script to quickly jump among directores in Unix/Linux shell

=head1 SYNOPSIS

jump.pl [options] [new_working_directory]

 Options:
    --action, -a    specify an action: add, search(default), prune, dump
    --help, -h, -?  brief help message
    --man, -m       full documentation
    --version, -v   version information

=head1 OPTIONS

=over 8

=item B<--action>

Specified which action to take, default action is search.

=over 8

=item B<--action add [ -f old_PWD ] PWD>

Add a history entry, jump from old_PWD to PWD directory.

=item B<--action search [pattern]>

Search a history entry with pattern in Perl regexp grammar.

=item B<--action prune>

Prune history entries which corresponding directores don't exist any more.

=item B<--action dump>

Only for debug purpose, dump the jump history.

=back

=item B<--help>

Print a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<--version>

Prints version information and exits.

=back

=head1 DESCRIPTION

B<jump.pl> stores its history entires in $HOME/.jump_history with Perl's
Storable module.

It's better use this script with its companion shell script "jump.sh".

Installation:

   export PATH=$PATH:$HOME/bin
   mv jump.pl $HOME/bin/
   . jump.pl

Usage:

   j /home
   j /boot
   j            # select in all history entries
   j me         # select in history entries filter by a pattern

=head1 LICENSE

This script is licensed under GPL v3.

=head1 AUTHOR

Liu Yubao <yubao.liu at gmail.com>

=head1 SEE ALSO

autojump <http://wiki.github.com/joelthelion/autojump/>

=cut

