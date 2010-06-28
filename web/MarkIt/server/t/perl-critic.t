#!/usr/bin/perl

# Test that the module passes perlcritic
use strict;
BEGIN {
	$|  = 1;
	$^W = 1;
}
use File::Find;

my @MODULES = (
	'Perl::Critic 1.098',
	"Test::Perl::Critic 1.01 (-profile => 't/perlcriticrc')",
);

# Don't run tests during end-user installs
use Test::More;
unless ( $ENV{AUTOMATED_TESTING} or $ENV{RELEASE_TESTING} ) {
	plan( skip_all => "Author tests not required for installation" );
}

# Load the testing modules
foreach my $MODULE ( @MODULES ) {
	eval "use $MODULE";
	if ( $@ ) {
		$ENV{RELEASE_TESTING}
		? die( "Failed to load required release-testing module $MODULE" )
		: plan( skip_all => "$MODULE not available for testing" );
	}
}

my @files;

File::Find::find(
    {
        wanted  => sub {
            /^.*\.pm\z/msx
                && $File::Find::dir !~ /templates/mx
                && push @files, $File::Find::name;
            }
    },
    'blib/'
);

foreach my $file (@files) {
    critic_ok($file);
}

done_testing(scalar @files);

1;
