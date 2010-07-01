package MarkIt::Mark;

use DBI qw(:sql_types);
use JSON;
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

    my ($dbh, $sth, $rv);

    $dbh = $c->dbh;
    $dbh->begin_work;

    $sth = $dbh->prepare("INSERT INTO marks VALUES (?, ?, ?, ?, ?, ?)");
    $sth->bind_param(1, $left, SQL_INTEGER);
    $sth->bind_param(2, $top, SQL_INTEGER);
    $sth->bind_param(3, $key, SQL_VARCHAR);
    $sth->bind_param(4, $url, SQL_VARCHAR);
    $sth->bind_param(5, $title, SQL_VARCHAR);
    $sth->bind_param(6, $marks, SQL_VARCHAR);
    $rv = $sth->execute();

    if ($rv) {
        $c->log->info("add mark: insert successfully!\n");
        $dbh->commit;
    } else {
        $sth = $dbh->prepare("UPDATE OR FAIL marks SET left=?, top=?, title=?, marks=? WHERE key=? AND url=?");
        $sth->bind_param(1, $left, SQL_INTEGER);
        $sth->bind_param(2, $top, SQL_INTEGER);
        $sth->bind_param(3, $title, SQL_VARCHAR);
        $sth->bind_param(4, $marks, SQL_VARCHAR);
        $sth->bind_param(5, $key, SQL_VARCHAR);
        $sth->bind_param(6, $url, SQL_VARCHAR);
        $rv = $sth->execute();

        if ($rv) {
            $c->log->info("add mark: update successfully!\n");
            $dbh->commit;
        } else {
            $c->log->error("add mark: update failed!\n");
            $dbh->rollback;
        }
    }

    if ($rv) {
        return "ok";
    } else {
        return "bad";
    }
}

sub view {
    my ($c) = @_;

    my $q = $c->query;
    my $key = $q->param("key");
    my $url = $q->param("url");

    $c->log->info("view mark: key=[$key], url=[$url]\n");

    my ($dbh, $sth, $rv);

    $dbh = $c->dbh;

    $sth = $dbh->prepare("SELECT left, top, marks FROM marks WHERE key=? AND url=?");
    $sth->bind_param(1, $key, SQL_VARCHAR);
    $sth->bind_param(2, $url, SQL_VARCHAR);
    $rv = $sth->execute();

    if ($rv) {
        my $row = $sth->fetch;
        $row = [] if ! $row;
        return to_json($row, {utf8 => 1});
    } else {
        return "[]";
    }
}

1;

