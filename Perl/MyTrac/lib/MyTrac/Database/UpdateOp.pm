package MyTrac::Database::UpdateOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock :seek/;
use MyTrac::Database::Operation;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Database::Operation';

has 'data'      => (is => 'ro', isa => 'Str', required => 1);

sub prepare {
    my ($self) = @_;

    sysopen my $fh, $self->db->git_path($self->filename), O_WRONLY or
            $self->throw("Can't open " . $self->filename . " to write", $!);

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or $self->throw("Can't lock EX on " .  $self->filename, $!);
    $self->locked(1);
}

sub execute {
    my ($self) = @_;
    my $data = $self->data;
    utf8::downgrade($data) or $self->throw("Not utf-8 encoding!");

    truncate $self->fh, 0 or $self->throw("Can't truncate " . $self->filename, $!);
    seek $self->fh, 0, SEEK_SET or $self->throw("Can't seek " .  $self->filename, $!);

    syswrite($self->fh, $data) == length($data) or
            $self->throw("Failed to write " . $self->filename, $!);
}

sub rollback {
    my ($self) = @_;

    my @cmd = $self->db->git_cmd(qw/checkout HEAD --/, $self->filename);
    system(@cmd) or $self->throw("Can't reset " . $self->filename);
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
