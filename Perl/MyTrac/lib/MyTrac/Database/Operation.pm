package MyTrac::Database::Operation;
use Any::Moose;
use Carp;
use Fcntl qw/:flock/;
use File::Spec;
use MyTrac::Exceptions;
use namespace::autoclean;

our $VERSION = '0.01';

has 'id'        => (is => 'ro', isa => 'Str', required => 1);
has 'db'        => (is => 'ro', isa => 'MyTrac::Database', required => 1);
has 'lock_mode' => (is => 'ro', isa => 'Int', default => LOCK_SH);
has 'filename'  => (is => 'rw', isa => 'Str');
has 'fh'        => (is => 'rw', isa => 'GlobRef');
has 'locked'    => (is => 'rw', isa => 'Bool', default => 0);
has 'successful'=> (is => 'rw', isa => 'Bool', default => 0);

sub BUILD {
    my ($self) = @_;

    my $dir = substr($self->id, 0, 2);
    $self->filename(File::Spec->catfile($dir, substr($self->id, 2)));

    $dir = $self->db->git_path($dir);
    if (! -e $dir && ! mkdir($dir, 0755) && ! $!{EEXIST}) {
        $self->throw("Can't create directory " . $dir, $!);
    }
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

sub throw {
    my ($self, $msg, $errno) = @_;

    my %params = (id => $self->id, message => $msg);
    my $operation = $self->meta->name;
    $operation =~ s/^.*Database:://;
    $operation =~ s/Op$//;

    $params{operation} = lc $operation;
    $params{errno} = $errno if defined $errno;
    $params{show_trace} = 1;

    MyTrac::Database::OperateException->throw(%params);
}

# require Mouse >= 0.51:
#   http://cpansearch.perl.org/src/GFUJI/Mouse-0.59/Changes
#       0.51 Mon Mar 15 15:25:58 2010
#       * Mouse::Object::DESTROY could cause SEGVs
sub DEMOLISH {
    my ($self) = @_;

    if (! $self->successful) {
        local $@;   # DON'T clear global $@ by this "eval"
        eval {
            $self->rollback if $self->locked;
        };
        Carp::cluck $@ if $@;

        my $dir = substr($self->id, 0, 2);
        $dir = $self->db->git_path($dir);
        rmdir $dir;
    }

    if (defined($self->fh)) {
        if ($self->locked) {
            flock($self->fh, LOCK_UN) or
                    Carp::cluck("Can't unlock(" . $self->_lock_mode . ") " .  $self->filename);
            $self->locked(0);
        }

        close($self->fh) or Carp::cluck("Can't close " .  $self->filename);
        $self->fh(undef);
    }
}

sub _lock_mode {
    my ($self) = @_;

    ($self->lock_mode & LOCK_EX) == LOCK_EX ? "LOCK_EX" : "LOCK_SH";
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
