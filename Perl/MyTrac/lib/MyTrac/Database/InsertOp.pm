package MyTrac::Database::InsertOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use namespace::autoclean;

our $VERSION = '0.01';

has 'data'      => (is => 'ro', isa => 'Str', required => 1);

sub prepare {
    my ($self) = @_;

    sysopen my $fh, File::Spec->catfile($self->db->work_tree, $self->filename), O_WRONLY | O_CREAT | O_EXCL;
    confess "Can't create " . $self->filename . " to write: $!" if !defined $fh;

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or confess "Can't lock EX on " .  $self->filename . ": $!";

    confess "Other process has written " . $self->filename if 0 != (stat($fh))[7];
}

sub execute {
    my ($self) = @_;
    my $data = $self->data;
    utf8::downgrade($data) or confess "Not utf-8 encoding!";

    syswrite($self->fh, $data) == length($data) or
            confess "Failed to write " . $self->filename . ":$!";
}

sub rollback {
    my ($self) = @_;

    unlink File::Spec->catfile($self->db->work_tree, $self->filename) or
            confess "Can't unlink " . $self->filename . ":$!";
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
