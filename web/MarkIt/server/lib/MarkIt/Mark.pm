package MarkIt::Mark;

use base 'MarkIt::Base';

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/add view/]);
}

sub add {
    my ($c) = @_;

    my $q = $c->query;
    my $left = $q->param("left");
    my $top = $q->param("top");
    my $key = $q->param("key");
    my $url = $q->param("url");
    my $title = $q->param("title");
    my @marks = $q->param("mark");
    my $marks = join("\n", @marks);

    $c->log->info("add mark: ($left, $top), key=[$key], url=[$url], title=[$title]\n");
    $c->log->info("          marks:\n$marks\n");

    return "ok";
}

sub view {
    my ($c) = @_;

    return "hello world";
}

1;

