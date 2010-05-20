package MyTrac::Database::DeleteOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use namespace::autoclean;

our $VERSION = '0.01';

sub prepare {
    my ($self) = @_;

    sysopen my $fh, $self->db->git_path($self->filename), O_WRONLY;
    confess "Can't open " . $self->filename . " to write: $!" if !defined $fh;

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or confess "Can't lock EX on " .  $self->filename . ": $!";
}

sub execute {
    my ($self) = @_;

    truncate $self->fh, 0 or confess "Can't truncate " . $self->filename .  ": $!";
}

sub rollback {
    my ($self) = @_;

    my @cmd = $self->db->git_cmd(qw/checkout --/, $self->filename);
    system(@cmd) or confess "Can't reset " . $self->filename;
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
