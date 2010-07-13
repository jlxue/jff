package MarkIt::Mark;

use DBI 1.40 qw(:sql_types);    # 1.40 introduces $if_active=3 in $dbh->prepare_cached()
use JSON;
use MarkIt::Util qw/trim/;
use base 'MarkIt::Base';

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/add view/]);
}

sub cgiapp_prerun {
    my ($c) = @_;

    $c->SUPER::cgiapp_prerun(@_);

    $c->header_add(-expires         => "-1",
                   -Cache_Control   => "must-revalidate, no-cache, no-store",
                   -Pragma          => "no-cache");
}

sub add {
    my ($c) = @_;

    my $q = $c->query;
    my $key = $q->param("key");
    my $url = $q->param("url");
    my $title = $q->param("title");
    my @marks = $q->param("mark");
    my $tags = $q->param("tags");
    my $left = $q->param("left");
    my $top = $q->param("top");

    trim($key, $url, $title, @marks, $tags, $left, $top);

    my $marks = join("\n", @marks);

    $c->log->info("add mark: tags=[$tags]\n");
    $c->log->info("add mark: ($left, $top), key=[$key], url=[$url], title=[$title]\n");
    $c->log->info("          marks:\n$marks\n");

    my ($dbh, $sth, $rv);

    $dbh = $c->dbh;
    $dbh->begin_work;

    $sth = $dbh->prepare_cached("INSERT INTO marks VALUES (?, ?, ?, ?, ?, ?, ?)", {}, 3);
    $sth->bind_param(1, $key, SQL_VARCHAR);
    $sth->bind_param(2, $url, SQL_VARCHAR);
    $sth->bind_param(3, $title, SQL_VARCHAR);
    $sth->bind_param(4, $marks, SQL_VARCHAR);
    $sth->bind_param(5, $tags, SQL_VARCHAR);
    $sth->bind_param(6, $left, SQL_INTEGER);
    $sth->bind_param(7, $top, SQL_INTEGER);
    $rv = $sth->execute();

    if ($rv) {
        $c->log->info("add mark: insert successfully!\n");
        $dbh->commit;
    } else {
        $sth = $dbh->prepare_cached("UPDATE OR FAIL marks SET left=?, top=?, title=?, marks=?, tags=? WHERE key=? AND url=?", {}, 3);
        $sth->bind_param(1, $left, SQL_INTEGER);
        $sth->bind_param(2, $top, SQL_INTEGER);
        $sth->bind_param(3, $title, SQL_VARCHAR);
        $sth->bind_param(4, $marks, SQL_VARCHAR);
        $sth->bind_param(5, $tags, SQL_VARCHAR);
        $sth->bind_param(6, $key, SQL_VARCHAR);
        $sth->bind_param(7, $url, SQL_VARCHAR);
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
        return '"ok"';
    } else {
        return '"bad"';
    }
}

sub view {
    my ($c) = @_;

    my $q = $c->query;
    my $key = $q->param("key");
    my $url = $q->param("url");

    trim($key, $url);

    $c->log->info("view mark: key=[$key], url=[$url]\n");

    my ($dbh, $sth, $rv);

    $dbh = $c->dbh;

    $sth = $dbh->prepare_cached("SELECT left, top, marks, tags FROM marks WHERE key=? AND url=?", {}, 3);
    $sth->bind_param(1, $key, SQL_VARCHAR);
    $sth->bind_param(2, $url, SQL_VARCHAR);
    $rv = $sth->execute();

    if ($rv) {
        my $row = $sth->fetchrow_hashref;
        $row = {} if ! $row;
        return to_json($row, {utf8 => 1});
    } else {
        return "{}";
    }
}

1;

