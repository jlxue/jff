package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str');
has 'depend'    => (is => 'ro', isa => 'ArrayRef[Str]');
has 'notify'    => (is => 'ro', isa => 'ArrayRef[Str]');
has 'stash'     => (is => 'rw', isa => 'HashRef');

sub needRepair {
    return 1;
}

sub isRepaired {
    return 0;
}

sub isKept {
    return 0;
}

sub saveState {
    my ($self, $isBeforeExecute) = @_;
}

sub execute {
}

sub rollback {
}

__PACKAGE__->meta->make_immutable();
