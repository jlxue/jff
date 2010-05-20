package MyTrac::Database::InsertOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use namespace::autoclean;

our $VERSION = '0.01';

has 'data'      => (is => 'ro', isa => 'Str', required => 1);
# has this data been added by others?
has 'added'     => (is => 'rw', isa => 'Bool', default => 0);

sub prepare {
    my ($self) = @_;
    my $file = $self->db->git_path($self->filename);

    sysopen my $fh, $file, O_WRONLY | O_CREAT | O_EXCL;
    if (! defined $fh) {
        my $old_error = $!;

        # may the file is a empty file produced by DeleteOp
        sysopen($fh, $file, O_WRONLY) or confess "Can't open " . $self->filename . " to write: $!";
    }

    $self->fh($fh);
    flock($fh, LOCK_EX | LOCK_NB) or confess "Can't lock EX on " .  $self->filename . ": $!";

    if (0 != (stat($fh))[7]) {
        $self->added(1);
        confess "Other process has written " . $self->filename
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
        unlink $self->db->git_path($self->filename) or
                confess "Can't unlink " . $self->filename . ":$!";
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
