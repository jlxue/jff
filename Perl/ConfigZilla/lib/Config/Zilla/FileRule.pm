package Config::Zilla::FileRule;
use strict;
use warnings;
use Any::Moose;
use File::Spec;

extends 'Config::Zilla::Rule';

has 'path'      => (is => 'ro', isa => 'Str', required => 1);

sub validate {
    my ($self) = @_;

    $self->SUPER::validate();

    die 'Path must be absolute' if ! File::Spec->file_name_is_absolute($self->path);
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
