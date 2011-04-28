package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str');

# Depends on which rules
has 'depend'    => (is => 'ro', isa => 'ArrayRef[Str]');

# { ruleName1 => [ eventName, args ] }
has 'notify'    => (is => 'ro', isa => 'HashRef');
# { eventName => code }
has 'listen'    => (is => 'ro', isa => 'HashRef');

# Extra expectation
has 'expect'    => (is => 'ro', isa => 'HashRef');

# Only execute this rule after 'ifelapsed' seconds
has 'ifelapsed' => (is => 'ro', isa => 'Int');

no Any::Moose;
__PACKAGE__->meta->make_immutable();
