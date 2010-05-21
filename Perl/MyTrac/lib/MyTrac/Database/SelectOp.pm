package MyTrac::Database::SelectOp;
use Any::Moose;
use Carp;
use Fcntl qw/:DEFAULT :flock/;
use MyTrac::Database::Operation;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Database::Operation';

has 'revision'  => (is => 'ro', isa => 'Str');

sub prepare {
    my ($self) = @_;

    return if defined $self->revision;

    sysopen my $fh, $self->db->git_path($self->filename), O_RDONLY;
    confess "Can't open " . $self->filename . " to read: $!" if !defined $fh;

    $self->fh($fh);
    flock($fh, LOCK_SH | LOCK_NB) or confess "Can't lock SH on " .  $self->filename . ": $!";
    $self->locked(1);
}

sub execute {
    my ($self) = @_;
    my $data;

    if (defined $self->revision) {
        my $file = $self->revision . ':' . $self->filename;
        my @cmd = $self->db->git_cmd('cat-file', 'blob', $file);
        open my $fh, join(' ', @cmd) . ' |' or confess "Can't git-cat-file $file: $!";
        local $/;
        $data = <$fh>;
        close $fh;
    } else {
        local $/;
        my $fh = $self->fh;
        $data = <$fh>;
    }

    confess "Invalid data!" if length($data) == 0;

    utf8::upgrade($data);

    return $data;
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
