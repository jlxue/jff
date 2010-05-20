package MyTrac::Database::DeleteOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use namespace::autoclean;

our $VERSION = '0.01';

# Is this file has been deleted by others?
has 'deleted'       => (is => 'rw', isa => 'Bool', default => 0);

sub prepare {
    my ($self) = @_;

    sysopen my $fh, $self->db->git_path($self->filename), O_WRONLY;
    confess "Can't open " . $self->filename . " to write: $!" if !defined $fh;

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or confess "Can't lock EX on " .  $self->filename . ": $!";

    if (0 == (stat($fh))[7]) {
        $self->deleted(1);
        confess "Has been deleted: " . $self->filename
    }
}

sub execute {
    my ($self) = @_;

    truncate $self->fh, 0 or confess "Can't truncate " . $self->filename .  ": $!";
}

sub rollback {
    my ($self) = @_;

    if (! $self->deleted) {
        my @cmd = $self->db->git_cmd(qw/checkout HEAD --/, $self->filename);
        system(@cmd) or confess "Can't reset " . $self->filename;
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
