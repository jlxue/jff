package MyTrac::Database::DeleteOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use MyTrac::Database::Operation;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Database::Operation';

sub prepare {
    my ($self) = @_;

    sysopen my $fh, $self->db->git_path($self->filename), O_WRONLY or
            $self->throw("Can't open " . $self->filename . " to write", $!);

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or $self->throw("Can't lock EX on " .  $self->filename, $!);
    $self->locked(1);
}

#sub execute {
    # nothing to do for file in work tree, we'll delete it when transaction is successful.
#}

sub rollback {
    my ($self) = @_;

    my @cmd = $self->db->git_cmd(qw/reset -q --/, $self->filename);
    system(@cmd) or $self->throw("Can't reset " . $self->filename);
}

sub DEMOLISH {
    my ($self) = @_;

    if ($self->successful) {
        unlink($self->db->git_path($self->filename)) or
                Carp::cluck "Can't remove " . $self->filename . ":$!";
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
