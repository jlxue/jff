#!/usr/bin/perl
#
# Usage:
#   aptitude install etckeeper
#   cd /etc && git -i -o --exclude-standard | sed -e '|^|/etc/|' | perl metastore-dump.pl /etc
#
# Note:
#   If the ignore list is big, it's better to use Regexp::List to optimize.
#
use File::Find;
use strict;
use warnings;

my @ignores = ();
if (! -t STDIN) {
    @ignores = <STDIN>;
    chomp @ignores;
    @ignores = sort @ignores;
}

find({wanted => \&wanted,
      preprocess => sub {
          return () if $File::Find::dir =~ m|/.git$|;
          sort @_;
      }
  }, @ARGV);

sub wanted {
    # XXX: It's better to filter files in preprocess function...
    for (my $i = 0; $i < @ignores; ++$i) {
        if ($File::Find::name eq $ignores[$i]) {
            splice @ignores, $i, 1;
            return;
        }
    }

    my @st = lstat;
    printf "mode=%06o ", $st[2];
    print "mtime=$st[9] ctime=$st[10] rdev=$st[6] uid=$st[4] gid=$st[5] nlink=$st[3] size=$st[7] ",
          $File::Find::name, "\n";
}

