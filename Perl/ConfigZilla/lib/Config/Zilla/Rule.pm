package Config::Zilla::Rule;
use strict;
use warnings;
use utf8;
use Any::Moose;
use constant RULE_NAME_REGEXP   => qr/^[[:alnum:]][[:alnum:]_-]*$/;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str', default => '');

# Depends on which rules
has 'depends'   => (is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] });

# Only execute this rule after 'ifelapsed' seconds
has 'ifelapsed' => (is => 'ro', isa => 'Int', default => 0);

has 'maxtime'   => (is => 'ro', isa => 'Int', default => 0);

has 'executor'  => (is => 'ro', isa => 'Str');

sub BUILD {
    my ($self, $args) = @_;
    confess 'Use "depends" not "depend"' if defined $args && exists $args->{depend};
    confess 'Invalid rule name' if $self->name !~ RULE_NAME_REGEXP;

    my @deps = @{ $self->depends };
    for my $dep (@deps) {
        confess 'Invalid dependent name' if $dep !~ RULE_NAME_REGEXP;
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
