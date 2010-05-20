package MyTrac::Database::Operation;
use Any::Moose;
use Carp;
use Fcntl qw/:flock/;
use File::Spec;
use namespace::autoclean;

our $VERSION = '0.01';

has 'id'        => (is => 'ro', isa => 'Str', required => 1);
has 'data'      => (is => 'ro', isa => 'Str', required => 1);
has 'lock_mode' => (is => 'ro', isa => 'Int', default => LOCK_SH);
has 'filename'  => (is => 'rw', isa => 'Str');
has 'fh'        => (is => 'rw', isa => 'FileHandle');
has 'successful'=> (is => 'rw', isa => 'Bool', default => 0);

sub BUILD {
    $self->filename(File::Spec->catfile(substr($self->id, 0, 2), substr($self, 2)));
}

sub prepare {
}

sub execute {
}

sub rollback {
}

sub commit {
    my ($self) = @_;

    $self->successful(1);
}

# require Mouse >= 0.51:
#   http://cpansearch.perl.org/src/GFUJI/Mouse-0.59/Changes
#       0.51 Mon Mar 15 15:25:58 2010
#       * Mouse::Object::DESTROY could cause SEGVs
sub DEMOLISH {
    if (! $self->successful) {
        eval {
            $self->rollback;
        }
    }

    if (defined($self->fh)) {
        flock($self->fh, LOCK_UN) or
                Carp::cluck("Can't unlock(" . $self->_lock_mode . ") " .  $self->filename);

        close($self->fh) or Carp::cluck("Can't close " .  $self->filename);
    }
}

sub _lock_mode {
    my ($self) = @_;

    ($self->lock_mode & LOCK_EX) == LOCK_EX ? "LOCK_EX" : "LOCK_SH";
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
