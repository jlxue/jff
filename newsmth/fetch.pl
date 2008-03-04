#!/usr/bin/perl -w

use strict;
use warnings;
use Data::Dumper;
use DBI qw/:sql_types/;
use Encode;
use File::Slurp;
use LWP::Simple;
use POSIX;
use threads;
use Thread::Semaphore;

use constant DSN => "dbi:mysql:newsmth";
use constant USERNAME => "root";
use constant PASSWORD => "";
use constant BOARDURL => 'http://www.newsmth.net/bbsdoc.php?board=';
use constant MAX_THREAD => 10;

my $term_enc = 'utf8';
my ($dbh, $sth, $boards);
my (@threads, $thread);
my $sema = new Thread::Semaphore(MAX_THREAD);

some_init();

$dbh = create_dbh();
$sth = $dbh->prepare('SELECT * FROM boards');
$sth->execute;
$boards = $sth->fetchall_arrayref;
$dbh->disconnect;

print STDERR Dumper($boards);

for my $board (@$boards) {
    $sema->down;

    $thread = threads->create(\&fetch_post_list, $board);
    push @threads, $thread;

    sleep 2;
}

while ($thread = shift @threads) {
    $thread->join;
}



##############################################################################
#
sub parse_post_list {
    my ($content, $fromnum, $tonum, $fromid, $toid) = @_;

    my (@a, @result);
    my ($begin, $end) = (0, 0);

    # "var c = new docWriter('LinuxApp',392,16382,0,0,548,16411,'/groups/comp.faq/LinuxApp',1,1);"
    $begin = index($content, 'var c = new docWriter(');
    die if $begin < 0;
    $end   = index($content, ',\'', $begin);
    die if $end < 0;
    @a = substr($content, $begin, $end - $begin) =~ /,\d+,(\d+),\d+,\d+,\d+,(\d+)/;
    die if @a != 2;
    ($$fromnum, $$tonum) = @a;
    ($$fromid, $$toid) = (INT_MAX, -1);

    # "c.o(542605,542393,'poorest','  ',1195144249,'Re: show一下我fedora 8启动的服务 ',115,0);"
    #($postId, $topicId, $author, $flags, $time, $title, $bytes, $n);
    while (($begin = index($content, 'c.o(', $begin)) >= 0) {
        $end = index($content, ");\n", $begin);
        last if $end <= $begin;
        @a = substr($content, $begin + 4, $end - $begin) =~
                /(\d+),(\d+),'(.*)','(.*)',(\d+),'(.*)',(\d+),(\d+)/;
        die if @a != 8;
        $$fromid = $a[0] if $$fromid > $a[0];
        $$toid   = $a[0] if $$toid < $a[0];
        push @result, [@a];
        $begin = $end;
    }

    die if ($$fromid > $$toid);

    return @result;
}


sub format_post {
    my $post = shift;
    my $result = sprintf "[%10s] %6d (%-12s) | %s",
                         strftime("%m-%d %H:%M", localtime($post->[4])), # time
                         $post->[0],    # postId
                         $post->[2],    # author
                         $post->[5];    # title
    return $result;
}


sub some_init {
    $| = 1;
    if ($^O eq "MSWin32" || 
            (exists($ENV{'LC_MESSAGES'}) && $ENV{'LC_MESSAGES'} =~ /\.GB(?:2312|K|18030)/i) ||
            (exists($ENV{'LANG'})        && $ENV{'LANG'}        =~ /\.GB(?:2312|K|18030)/i) ||
            (exists($ENV{'LC_CTYPE'})    && $ENV{'LC_CTYPE'}    =~ /\.GB(?:2312|K|18030)/i)) {
        $term_enc = 'gbk';
    }
}


sub fetch_post_list {
    my ($board) = @_;
    my $dbh = create_dbh();

    # from, to 都是帖子的 id, from 小于 to
    my ($id, $name, $from, $to) = @$board;
    # fromnum: 一页中的帖子最小显示序号
    # tonum: 此版面最后一篇帖子的显示序号，每页最多 30 个帖子
    # fromid, toid: 一页中帖子的最低、最高 id
    my ($fromnum, $tonum, $fromid, $toid);
    my ($content, @posts, $pagenum);

    print STDERR "fetching board [$name]...\n";

    # 取最后一页，以确定 (from, to) 在哪一页。
    $content = get(BOARDURL . $name);
    goto LEAVE unless defined $content;
    $content = decode('GBK', $content);
    @posts = parse_post_list($content, \$fromnum, \$tonum, \$fromid, \$toid);
    goto LEAVE if ($from > $toid);

    $to = $toid if ($to < 0 || $from > $to);
    $from = 1 if $from < 0;
    $pagenum = int(($tonum - ($toid - $from)) / 30);
    $pagenum = 1 if $pagenum < 1;
    $pagenum = int($tonum / 30) if $pagenum > int($tonum / 30);

    print STDERR "[$name]: from=$from, to=$to, num=($fromnum,$tonum),",
        "id=($fromid,$toid), pagenum=$pagenum\n";

    # pagenum 可能不准确，修正之
    while ($pagenum > 1) {
        sleep rand 5;

        $content = get(BOARDURL . $name . '&page=' . $pagenum);
        goto LEAVE unless defined $content;
        $content = decode('GBK', $content);
        @posts = parse_post_list($content, \$fromnum, \$tonum, \$fromid, \$toid);

        print STDERR "[$name]: from=$from, to=$to, num=($fromnum,$tonum),",
            "id=($fromid,$toid), pagenum=$pagenum\n";

        if ($from < $fromid) {
            --$pagenum;
        } elsif ($from > $toid) {
            ++$pagenum;
            next;
        } else {
            last;
        }
    }

    $sth = $dbh->prepare("INSERT INTO posts values ($id, ?, ?, ?, ?, FROM_UNIXTIME(?), ?, ?, ?)");
    my $sth2 = $dbh->prepare("UPDATE boards SET fromPostID=?, toPostiD=? where id=$id");

    # 抓取 (from, to) 范围的帖子列表
    while ($from <= $to) {
        sleep rand 5;

        $content = get(BOARDURL . $name . '&page=' . $pagenum);
        last unless defined $content;
        $content = decode('GBK', $content);
        @posts = parse_post_list($content, \$fromnum, \$tonum, \$fromid, \$toid);

        print STDERR "[$name]: from=$from, to=$to, num=($fromnum,$tonum),",
            "id=($fromid,$toid), pagenum=$pagenum\n";

        $dbh->begin_work;
        for my $post (@posts) {
            if ($post->[0] >= $from && $from <= $to) {
                $from = $post->[0] + 1;
                print $name, ': ', encode($term_enc, format_post($post)), "\n";
                $sth->bind_param(1, $post->[0], SQL_INTEGER);       # id
                $sth->bind_param(2, $post->[1], SQL_INTEGER);       # topicID
                $sth->bind_param(3, $post->[2], SQL_VARCHAR);       # author
                $sth->bind_param(4, $post->[3], SQL_CHAR);          # flags
                $sth->bind_param(5, $post->[4], SQL_DATETIME);      # time
                $sth->bind_param(6, $post->[5], SQL_VARCHAR);       # title
                $sth->bind_param(7, $post->[6], SQL_INTEGER);       # length
                $sth->bind_param(8, $post->[7], SQL_INTEGER);       # n
                $sth->execute;
            }
        }
        $sth2->bind_param(1, $from, SQL_INTEGER);
        $sth2->bind_param(2, $to, SQL_INTEGER);
        $sth2->execute;
        $dbh->commit;

        ++$pagenum;
    } 

LEAVE:
    $dbh->disconnect;
    $sema->up;
}


sub create_dbh {
    my $dbh = DBI->connect(DSN, USERNAME, PASSWORD,
        {RaiseError => 1, AutoCommit => 1});

    return $dbh;
}

