#!/usr/bin/perl
use Expect;
use IO::Pty;
use strict;
use warnings;
use constant TIMEOUT => 10;

if (@ARGV == 0) {
    print "Usage: $0 ssh username\@bbs.newsmth.net\n";
    exit;
}


my $inobject = Expect->exp_init(\*STDIN);
my $outobject = Expect->exp_init(\*STDOUT);

$inobject->manual_stty(1);
$inobject->raw_pty(1);
$inobject->stty(qw(raw -echo));
$inobject->log_stdout(0);

$outobject->manual_stty(1);
$outobject->raw_pty(1);
$outobject->stty(qw(raw -echo));
$outobject->log_stdout(0);


my $bbs = new Expect;
$Expect::Debug = 3;
$Expect::Exp_Internal = 1;
$bbs->debug(3);
$bbs->exp_internal(1);
$bbs->manual_stty(1);
$bbs->raw_pty(1);
$bbs->stty(qw(raw -echo));
$bbs->log_stdout(0);
$bbs->log_file("logbbs.log", "w");

$bbs->slave->clone_winsize_from(\*STDIN);
$SIG{WINCH} = \&sigwinch_handler;

$bbs->restart_timeout_upon_receive(1);
$bbs->max_accum(100 * 1024);

$bbs->spawn(@ARGV);


# output from ssh or telnet goes to STDOUT
$bbs->set_group($outobject);
# input from STDIN goes to ssh or telnet
$inobject->set_group($bbs);


main_loop();
exit;

END {
    $inobject->raw_pty(0);
    $inobject->stty(qw(sane));
    $outobject->raw_pty(0);
    $outobject->stty(qw(sane));
    $bbs->hard_close();
    $inobject->hard_close();
    $outobject->hard_close();
}

######################################################################
sub sigwinch_handler {
    my $signame = shift;
    my $pid = $bbs->pid;

    $bbs->slave->clone_winsize_from(\*STDIN);
    kill WINCH => $pid if $pid;
}


sub main_loop {
    my @common_patterns = (
       [ 'eof'      , \&cb_cleanup ],
       [ 'timeout'  , \&cb_timeout ],
    );

    my @bbs_patterns = (
        [ 'hello'   , sub { $bbs->print_log_file("got hello\n"); } ],
        [ '-re', '^(\w+)\s*\((.*)\)\s*\r?\n?(.*)第 \d+ 条消息 / 共 \d+ 条消息\r?$'
                    , \&cb_newmessage ],
    );

    my @stdin_patterns = (
        #[ 'hello'   , sub { $bbs->print_log_file("stdin: got hello\n"); } ],
    );


    while (1) {
        my @result = expect(TIMEOUT,
            '-i', [$bbs, $inobject], @common_patterns,
            '-i', $bbs, @bbs_patterns,
        );

        if (defined $result[1] && $result[1] ne '1:TIMEOUT') {
            $bbs->print_log_file("\n\nDEBUG: got error: $result[1]!\n");
            last;
        }
    }
}


sub cb_cleanup {
}


sub cb_timeout {
    $bbs->print_log_file("exp objects: bbs=$bbs, inobject=$inobject, outobject=$outobject\n");
    $bbs->print_log_file("timeout: @_\n");
}

sub cb_newmessage {
    $bbs->print_log_file("got new message:{{{", $bbs->match(), "}}}\n");
}

