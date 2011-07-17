package Config::Zilla::Constants;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = 0.1;
our @EXPORT_OK;
our %EXPORT_TAGS;

my $i = 2;

use constant {
    EC_OK               => 0,   # EXIT_SUCCESS, no change needed
    EC_FAIL_UNKNOWN     => 1,   # EXIT_FAILURE

    EC_FAIL_LOCK        => $i++,    # Engine fails to obtain run lock
    EC_FAIL_START       => $i++,    # Engine fails to start executor

    EC_FAIL_PREREQ      => $i++,    # Some prerequisites fail
    EC_FAIL_PREPARE     => $i++,    # Error happened in prepare stage, no rollback needed
    EC_FAIL_APPLY       => $i++,    # Error happened in apply stage but rollback successfully
    EC_FAIL_ROLLBACK    => $i++,    # Error happened in apply and rollback state

    EC_FAIL_TIMEOUT     => $i++,    # Executor doesn't finish in specified timeout

    EC_APPLIED          => $i++,    # Applied successfully
};

my @rc = grep(/^EC_/, keys %Config::Zilla::Constants::);
push @EXPORT_OK, @rc;
$EXPORT_TAGS{EXIT_CODE} = \@rc;

$EXPORT_TAGS{ALL} = \@EXPORT_OK;

1;
