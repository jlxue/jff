package MyTrac::FileLock;
use Any::Moose;
use Fcntl qw/:flock/;
use namespace::autoclean;

our $VERSION = '0.01';

has fh      => (is => 'ro', isa => 'FileHandle', required => 1);
has mode    => (is => 'ro', isa => 'Int', required => 1);
has filename=> (is => 'ro', isa => 'Str', required => 1);

sub BUILD {
    my ($self, $args) = @_;

    flock($self->fh, $self->mode | LOCK_NB) or die "Can't lock " .  $self->filename .
            "(" . ($self->mode == LOCK_EX ? "LOCK_EX" : "LOCK_SH") . "): $!\n";
}

sub DESTROY {
    my ($self) = @_;

    flock($self->fh, LOCK_UN) or die "Can't unlock " . $self->filename .
            "(" . ($self->mode == LOCK_EX ? "LOCK_EX" : "LOCK_SH") . "): $!\n";
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
