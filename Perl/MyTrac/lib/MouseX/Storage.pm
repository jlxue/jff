package MouseX::Storage;
use JSON;
use Mouse::Role;
use constant JSON_OPTS => {canonical => 1, pretty => 1};
use namespace::autoclean;

our $VERSION = '0.01';

sub pack {
    my ($self) = @_;
    my $h = {};

    $h->{__CLASS__} = $self->meta->name . "-" . $VERSION;
    my @attributes = $self->meta->get_all_attributes;

    for my $attr (@attributes) {
        if ($attr->has_value($self)) {
            $h->{$attr->name} = $attr->get_value($self);
        }
    }

    return $h;
}

sub unpack {
    my ($class, $h) = @_;

    if (exists $h->{__CLASS__}) {
        #confess 'Bad class or version!' if $h->{__CLASS__} ne
        #        $class . "-" . ${${class}::VERSION};
        delete $h->{__CLASS__};
    }

    $class->new($h);
}

sub freeze {
    my ($self, @args) = @_;

    to_json($self->pack(@args), JSON_OPTS);
}

sub thaw {
    my ($class, $json, @args) = @_;

    $class->unpack(from_json($json, JSON_OPTS), @args);
}

sub load {
}

sub store {
}

no Mouse::Role;
1;
