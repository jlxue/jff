package MyTrac;

use base 'Titanium';
use Carp;
use MyTrac::Article;
use MyTrac::Database;
use strict;
use warnings;
use constant MODE_PARAM => 'm';

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;
    my $db = new MyTrac::Database({
            git_dir     => $c->param('git_dir'),
            work_tree   => $c->param('work_tree'),
        });
    $c->{__MYTRAC_DB} = $db;

    $c->mode_param(MODE_PARAM);
    $c->start_mode('list');
    $c->run_modes([qw/list insert update delete select/]);
}

sub list {
    my ($c) = @_;
    my ($db, $q, $s) = ($c->db, $c->query, $c->session);

    if ($q->request_method ne 'GET') {
        return;
    }
}

sub insert {
    my ($c) = @_;
    my ($db, $q, $s) = ($c->db, $c->query, $c->session);

    if ($q->request_method ne 'POST') {
        return;
    }

    my $title = $q->param('title');
    my $content = $q->param('content');
    my $tags = $q->param('tags');

    if (!defined $title or !defined $content) {
        return;
    }

    $title =~ s/^\s+|\s+$//g;
    if (length($title) == 0) {
        return;
    }

    my $article = new MyTrac::Article({title => $title, content => $content});
    if (defined $tags) {
        $tags =~ s/^\s+|\s+$//g;
        $tags =~ s/\s*,+\s*/,/g;
        $tags =~ s/^,+|,+$//g;

        $article->tags($tags) if length($tags) > 0;
    }

    eval {
    }
}

sub update {
    my ($c) = @_;
    my ($db, $q, $s) = ($c->db, $c->query, $c->session);

    if ($q->request_method ne 'POST') {
        return;
    }
}

sub delete {
    my ($c) = @_;
    my ($db, $q, $s) = ($c->db, $c->query, $c->session);

    if ($q->request_method ne 'POST') {
        return;
    }
}

sub select {
    my ($c) = @_;
    my ($db, $q, $s) = ($c->db, $c->query, $c->session);
    my @ids;

    if ($q->request_method ne 'GET') {
        return;
    }
}

sub db {
    my ($c) = @_;

    return $c->{__MYTRAC_DB};
}

1;

__END__
