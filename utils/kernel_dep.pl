#!/usr/bin/perl -w
#
# Purpose:
#   show target dependency graph of Linux kernel building.
#
# Usage:
#   make mrproper
#   make allnoconfig
#   make V=1 2>&1 | tee make.log
#   perl kernel_dep.pl make.log dep.xml
#   firefox dep.xml
#
#   Without first argument, kernel_dep.pl reads from stdin,
#   without second argument, kernel_dep.pl writes to stdout.
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# License:
#   GPL v2
#
# ChangeLog:
#   2008-09-17  Liu Yubao
#       * initial version
#   2008-09-19  Liu Yubao
#       * add header comment
# 
use strict;
use warnings;
use Data::Dumper;
use Tk;

my @a;
my $indent = 0;

my %depends = ();
my @targets = ();

if (@ARGV > 0) {
    close STDIN;
    open STDIN, $ARGV[0] || die "Can't read $ARGV[0]: $!";
}

if (@ARGV > 1) {
    close STDOUT;
    open STDOUT, ">", $ARGV[1] || die "Can't write $ARGV[1]: $!";
}

while (<STDIN>) {
    if (/^\s*ld\s/) {
        chomp;
        @a = split;
        parse_ld_cmd(@a);
    } elsif (/^\s*objcopy\s/) {
        chomp;
        @a = split;
        parse_objcopy_cmd(@a);
    } elsif (/\/build\s/) {
        chomp;
        @a = split;
        add_dependency($a[$#a], @a[$#a - 4 .. $#a - 3]);
    } elsif (/\bgzip\b/) {
        chomp;
        @a = split;
        add_dependency($a[$#a], $a[$#a - 2]);
    }
}

@targets = reverse @targets;

#print Dumper(\%depends);
dump_xml();

#-----------------------------------------------------------------------
# show @targets and %depends with Tk::BrowseEntry and Tk::Tree
#

my $selected_target = $targets[0];
my @sorted_targets = sort keys %depends;
my $mw = new MainWindow();
my $be = $mw->BrowseEntry(-label => "Target:", -variable => \$selected_target,
                          -autolistwidth => 1, -choices => \@sorted_targets,
                          -state => "readonly", -browsecmd => \&onBrowseChange);
my $tree = $mw->Scrolled(qw/Tree -separator @ -width 60 -height 40 -selectmode extended -scrollbars se/);

fill_tree("", $selected_target);

$be->pack(-fill => 'x');
$tree->pack(-fill => 'both', -expand => 1);

MainLoop;

########################################################################

sub add_dependency {
    my $target = shift;

    $depends{$target} = [ @_ ];
    push @targets, $target;
}


sub parse_ld_cmd {
    my $target = "XXX";
    my @objs = ();

    shift;      # remove "ld" command

    for (my $i = 0; $i <= $#_; ++$i) {
        if ('-o' eq $_[$i]) {
            $target = $_[++$i];
        } elsif ('-T' eq $_[$i] || '-m' eq $_[$i] ||
                '--format' eq $_[$i] || '--oformat' eq $_[$i]) {
            ++$i;
        } elsif ('-r' eq $_[$i] || '--build-id' eq $_[$i] ||
                '--start-group' eq $_[$i] || '--end-group' eq $_[$i]) {
            # omit
        } else {
            push @objs, $_[$i];
        }
    }

    add_dependency($target, @objs);
}


sub parse_objcopy_cmd {
    add_dependency($_[$#_], $_[$#_ - 1]);
}


sub fill_tree {
    my ($parent, $target) = @_;

    my $entry = $tree->addchild($parent, -text => $target);

    return if ! exists $depends{$target};

    for my $t (@{$depends{$target}}) {
        fill_tree($entry, $t);
    }
}


sub onBrowseChange {
    $tree->delete("all");
    fill_tree("", $_[1]);
}


sub dump_xml {
    print "<targets>\n";
    for my $t (@targets) {
        dump_target($t);
    }
    print "</targets>\n";
}


sub dump_target {
    my $t = shift;

    print " " x $indent, "<o n=\"$t\">\n";

    if (exists $depends{$t}) {
        $indent += 2;
        my @objs = @{$depends{$t}};
        for my $obj (@objs) {
            dump_target($obj);
        }
        $indent -= 2;
    }

    print " " x $indent, "</o>\n";
}

