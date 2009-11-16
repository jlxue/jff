#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

my $graph = get_rev_graph();
my $leafs = get_leafs_of_graph($graph);

simplify_graph($graph, @$leafs);


######################################################################

# returns:
# {
#   commit1 => [seen, in-degree, parent1, parent2, ...],
#   ....
# }
# seen is a flag used to travel this graph, initialized to zero;
# in-degree(number of children) of commit1: $graph{commit1}->[1];
# out-degree(number of parents) of commit1: @{$graph{commit1}} - 2.
sub get_rev_graph {
    my %graph = ();

    open my $h, "git rev-list --parents --full-history --sparse --all |" or exit(1);
    while (<$h>) {
        chomp;
        my @a = split;          # commit, parent1, parent2...
        next if @a == 1;        # root commit

        if (exists $graph{$a[0]}) {
            die if @{$graph{$a[0]}} > 2;
            push @{$graph{$a[0]}}, @a[1..$#a];
        } else {
            $graph{$a[0]} = [0, 0, @a[1..$#a]];
        }


        for (@a[1..$#a]) {
            if (exists $graph{$_}) {
                $graph{$_}->[1]++;
            } else {
                $graph{$_} = [0, 1];
            }
        }
    }
    close $h;

    #print Dumper(\%graph);
    return \%graph;
}

# returns:
#   [ leaf1, leaf2....]
#
sub get_leafs_of_graph {
    my $graph = shift;
    my @leafs = ();

    while (my ($k, $v) = each %$graph) {
        push @leafs, $k if $v->[1] == 0;
    }

    # print Dumper(\@leafs);
    return \@leafs;
}


sub simplify_graph {
    my ($graph, @leafs) = @_;

    while (@leafs) {
        my $leaf = shift @leafs;
        my $info = $graph->{$leaf};     # [seen, in-degree, parent...]
        my $len = @$info;

        next if $info->[0];     # has been seen

        $info->[0]++;           # set flag `seen'

        if ($len == 3) {        # has only one parent
            my $parent = $info->[2];
            $info = $graph->{$parent};

            # skip parents that have one child and one parent
            while ($info->[1] == 1 && @$info == 3) {
                $parent = $info->[2];
                $info = $graph->{$parent};
            }

            print $leaf, " ", $parent, "\n";

            push @leafs, $parent;
        } elsif ($len == 2) {   # has no parent
            next;
        } else {                # has more than one parents
            print $leaf;
            for my $parent (@$info[2..$len - 1]) {
                print " ", $parent;
            }
            print "\n";

            push @leafs, @$info[2..$len - 1];
        }
    }
}

