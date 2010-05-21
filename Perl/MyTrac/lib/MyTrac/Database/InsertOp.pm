package MyTrac::Database::InsertOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use MyTrac::Database::Operation;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Database::Operation';

has 'data'      => (is => 'ro', isa => 'Str', required => 1);
# has this data been added by others?
has 'added'     => (is => 'rw', isa => 'Bool', default => 0);

sub prepare {
    my ($self) = @_;
    my $file = $self->db->git_path($self->filename);

    sysopen my $fh, $file, O_WRONLY | O_CREAT | O_EXCL, 0644 or
            confess "Can't create " . $self->filename . " to write: $!";

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or confess "Can't lock EX on " .  $self->filename . ": $!";
    $self->locked(1);

    if (0 != (stat($fh))[7]) {
        $self->added(1);
        confess "Other process has written " . $self->filename;
    }
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

    if (! $self->added) {
        system($self->db->git_cmd(qw/rm -f -q --cached --/, $self->filename)) or
            Carp::cluck "Can't git-rm --cached " . $self->filename .  ":$!";
        unlink $self->db->git_path($self->filename) or
                confess "Can't unlink " . $self->filename . ":$!";
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
