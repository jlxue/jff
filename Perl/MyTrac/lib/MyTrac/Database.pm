package MyTrac::Database;
use Any::Moose;
use Digest::SHA1 qw/sha1_hex/;
use File::Spec;
use File::Temp;
use JSON 2.0 qw//;
use namespace::autoclean;
#use Smart::Comments;

our $VERSION = '0.01';

has path        => (is => 'ro', isa => 'Str');
has json        => (is => 'rw', isa => 'Ref');

sub BUILD {
    my ($self, $args) = @_;

    $self->json(JSON->new->utf8->canonical->pretty->relaxed);

    if (! -e File::Spec->catdir($self->path, '.git')) {
        confess "Can't initialize database!" if
                0 != system('git', 'init', '-q', $self->path);
    }
}

sub pack {
    my ($self, $obj) = @_;

    my $h = {};
    $h->{__CLASS__} = $obj->meta->name . "-" . UNIVERSAL::VERSION($obj);
    my @attributes = $obj->meta->get_all_attributes;

    for my $attr (@attributes) {
        if ($attr->has_value($obj)) {
            $h->{$attr->name} = $attr->get_value($obj);
        }
    }

    return $h;
}

sub unpack {
    my ($self, $h, $class) = @_;

    if (exists $h->{__CLASS__}) {
        my ($c, $v) = split /-/, $h->{__CLASS__}, 2;

        confess "Bad class signature!" if (! defined $v);
        confess "Versions don't match!" if $v ne UNIVERSAL::VERSION($c);

        if (defined $class) {
            confess "Classes don't match!" if $class ne $c;
        } else {
            $class = $c;
        }

        delete $h->{__CLASS__};
    } elsif (!defined $class) {
        confess "Class not specified!";
    }

    $class->new($h);
}

sub to_json {
    my ($self, $perl) = @_;

    $self->json->encode($perl);
}

sub from_json {
    my ($self, $json) = @_;

    $self->json->decode($json);
}

sub begin_work {
}

sub commit {
}

sub rollback {
}

sub insert {
    my ($self, $obj) = @_;

    my $h = $self->pack($obj);
    delete $h->{qw/id revision/};

    my $json = $self->to_json($h);

    $obj->id(sha1_hex($json));
}

sub update {
    my ($self, $obj) = @_;
}

sub delete {
    my ($self, $id) = @_;
}

sub select {
    my ($self, $rules) = @_;
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
