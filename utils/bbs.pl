#!/usr/bin/perl
use Expect;
use IO::Pty;
use strict;
use warnings;
use constant TIMEOUT => 60;

if (@ARGV == 0) {
    print "Usage: $0 ssh username\@newsmth.net\n";
    exit;
}


# reference Expect.pm:interact()
my ($infile, $inobject, $outfile, $outobject);
$infile = new IO::File;
$outfile = new IO::File;
{
    no strict 'subs';
    $infile->IO::File::fdopen(STDIN, 'r');
    $outfile->IO::File::fdopen(STDOUT, 'w');
}
$inobject = Expect->exp_init($infile);
$outobject = Expect->exp_init($outfile);
$outobject->manual_stty(1);


my $bbs = new Expect;
#$bbs->debug(3);
#$bbs->exp_internal(1);
$bbs->log_file("logbbs.log", "w");
$bbs->raw_pty(1);
$bbs->slave->clone_winsize_from(\*STDIN);
$SIG{WINCH} = \&sigwinch_handler;
$bbs->restart_timeout_upon_receive(1);
$bbs->max_accum(100 * 1024);
$bbs->spawn(@ARGV);


# output from ssh or telnet goes to STDOUT
$bbs->set_group($outobject);
# input from STDIN goes to ssh or telnet
$inobject->set_group($bbs);
$bbs->manual_stty(1);
$bbs->log_stdout(0);
$inobject->log_stdout(0);


main_loop();
exit;


######################################################################
sub sigwinch_handler {
    my $signame = shift;
    my $pid = $bbs->pid;

    $bbs->slave->clone_winsize_from(\*STDIN);
    kill WINCH => $pid if $pid;
}


sub main_loop {
    my @result;
    my @bbs_patterns = (
        [ 'eof'  ,   sub { $bbs->soft_close(); exit; } ],
        [ 'timeout', sub { $bbs->print_log_file("{{{bbs timeout}}}"); } ],
    );

    while (1) {
        @result = expect(TIMEOUT,
                         '-i', $bbs, @bbs_patterns,
                         '-i', $inobject);
        $bbs->print_log_file("{{{got @result}}}\n");
    }
}

