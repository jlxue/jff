#!/usr/bin/perl -w
use strict;
use warnings;
use Data::Dumper;
use File::Slurp;
use JSON;
use NewSMTH::DB;

BEGIN {
    NewSMTH::DB::connect();
}

my $rs = new DBIx::Class::ResultSet('NewSMTH::Boards',
    {order_by => 'id'});

my @boards = $rs->all();

my @boards_perl = map {[$_->id(), $_->name()]} @boards;
my $boards_json = objToJson(\@boards_perl);

write_file('json_boards.txt', $boards_json);

for my $board (@boards) {
    my @posts_perl = ();
    my ($lowid, $highid) = (0, 0);
    $rs = new DBIx::Class::ResultSet('NewSMTH::Posts',
        {order_by => ['id', 'topicID'],
        where => {boardID => $board->id()}});
    while (my $row = $rs->next()) {
        push @posts_perl, [$row->id(), $row->topicID(),
                $row->author(), $row->flags(), $row->time(),
                $row->title(), $row->length(), $row->n()];
    }
    $lowid = $posts_perl[0]->[0];
    $highid = $posts_perl[$#posts_perl]->[0];

    my $posts_json = objToJson(\@posts_perl);
    my $filename = "json_" . $board->name() . "_$lowid-$highid.txt"; 
    print $filename, "\n";
    write_file($filename, $posts_json);
}


