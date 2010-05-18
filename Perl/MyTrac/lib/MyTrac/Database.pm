package MyTrac::Database;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

has db_path     => (is => 'ro', isa => 'Str');

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

sub insert {
    my ($self, $obj) = @_;

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
