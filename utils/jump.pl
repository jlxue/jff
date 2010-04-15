#!/usr/bin/perl -w
use strict;
use warnings;

use Cwd;
use Fcntl qw/:DEFAULT :flock/;
use File::Spec;
use Getopt::Long;
use Smart::Comments '###';
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
GetOptions("--from=s"    => \$g_opt_from_work_dir,
           "--action=s"  => \$g_opt_action,
          );
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

        print "Which directory? ";
        $choice = <STDIN>;
        chomp $choice;
        $choice =~ s/^\s+//;

        return undef if $choice =~ /^q/i;
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

