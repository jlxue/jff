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

    flock($self->fh, $self->mode) or die "Can't lock " .  $self->filename .
            "(" . $self->_lock_mode . "): $!\n";
}

# require Mouse >= 0.51:
#   http://cpansearch.perl.org/src/GFUJI/Mouse-0.59/Changes
#       0.51 Mon Mar 15 15:25:58 2010
#       * Mouse::Object::DESTROY could cause SEGVs
sub DEMOLISH {
    my ($self) = @_;

    #flock($self->fh, LOCK_UN) or warn "Can't unlock " . $self->filename .
    #        "(" . $self->_lock_mode . "): $!\n";

    close $self->fh or warn "Can't close " . $self->filename .
            "(" . $self->_lock_mode . "): $!\n";;
}

sub _lock_mode {
    my ($self) = @_;

    ($self->mode & LOCK_EX) == LOCK_EX ? "LOCK_EX" : "LOCK_SH";
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
