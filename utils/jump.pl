#!/usr/bin/perl -w
use strict;
use warnings;

use Cwd;
use File::Spec;
use Getopt::Long;
use Smart::Comments '###';
use Storable qw/lock_store lock_retrieve/;
use Term::ANSIColor;

use Class::Struct HistoryItem =>
    [
    access_time   => '$',
    hit_count     => '$',
    from_dirs     => '%',
    ];

use constant    HISTORY_FILE    => ".jump_history";

######################################################################
## global variables
my $g_history_file = File::Spec->catfile($ENV{'HOME'}, HISTORY_FILE);
my $g_history;              # hash of working directories to HistoryItems
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
GetOptions("--from=s"    => \$g_opt_from_work_dir,
           "--action=s"  => \$g_opt_action,
          );
$g_opt_from_work_dir = getcwd() if !defined $g_opt_from_work_dir;

######################################################################
## load history file
$g_history = lock_retrieve($g_history_file) if -f $g_history_file;
$g_history = {} if !defined $g_history;

######################################################################
## handle action
if (exists $g_action_handler{$g_opt_action}) {
    my $ret = $g_action_handler{$g_opt_action}->(@ARGV);

    if (! $ret and defined $g_history and $g_opt_action ne 'dump') {
        $ret = lock_store $g_history, $g_history_file;

        $ret = $ret ? 0 : 1;
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

    return 1 if !defined $to_work_dir or !-d $to_work_dir;

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

    return 0;
}

sub action_search {
    my $pattern = shift;
    my (@candidates, $choice);

    if (defined $pattern) {
        @candidates = grep /$pattern/, keys %$g_history;
    } else {
        @candidates = keys %$g_history;
    }

    return 1 if @candidates == 0;

    if (@candidates == 1) {
        $choice = 1;
    } else {
        $choice = choice_directory(@candidates);
    }

    return 1 if $choice =~ /^q/i;

    --$choice;
    action_add($candidates[$choice]);
    print $candidates[$choice], "\n";

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

sub choice_directory {
    my @candidates = sort @_;
    my $choice;

    do {
        open my $pipe, "|less -FRX" || die "Can't pipe to \"less\": $!\n";
        for (my $i = 0; $i <= $#candidates; ++$i) {
            print $i + 1, "\t", $candidates[$i], "\n";
        }
        close $pipe;

        print "Which directory? ";
        $choice = <STDIN>;
        chomp $choice;
        $choice =~ s/^\s+//;
    } while ($choice !~ /^q/i && ($choice <= 0 || $choice > @candidates));
}

