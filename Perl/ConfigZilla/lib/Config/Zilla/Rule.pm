package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str', default => '');

# Depends on which rules
has 'depend'    => (is => 'ro', isa => 'ArrayRef[Str]');

# { eventName => arg }
has 'notify'    => (is => 'ro', isa => 'HashRef');
# { ruleName => { eventName => code } }
has 'listen'    => (is => 'ro', isa => 'HashRef');

# Extra expectation
has 'expect'    => (is => 'ro', isa => 'HashRef');

# Only execute this rule after 'ifelapsed' seconds
has 'ifelapsed' => (is => 'ro', isa => 'Int', default => 0);

sub validate {
    my ($self) = @_;

    die 'Rule name must match /^\w+$/' if $self->name !~ /^\w+$/;

    my @deps = @{ $self->depend };
    for my $dep (@deps) {
        die 'Dependent must match /^\w+$/' if $dep !~ /^\w+$/;
    }

    my @eventNames = keys %{ $self->notify };
    for my $name (@eventNames) {
        die 'Event name must match /repaired|failed|kept/' if
            $name !~ /^\w+$/;
    }

    my %listeners = %{ $self->listen };
    while (my ($ruleName, $events) = each %listeners) {
        die 'Rule name must match /^\w+$/' if $ruleName !~ /^\w+$/;

        my @eventNames = keys %$events;
        for my $name (@eventNames) {
            die 'Event name must match /repaired|failed|kept/' if
                $name !~ /^\w+$/;
        }
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
