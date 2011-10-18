package Event::Worker;
use strict;
use warnings;
use Data::Dumper;
use Event::Plumber;
use JSON;
use LWP::UserAgent;

sub new {
    my $class = shift;

    my $plumber = Event::Plumber->new();
    my @response = $plumber->establish();

    die $response[1] if $response[0] ne "ok";

    shift @response;
    my ($inputs, $outputs, $args) = @response;

    my $self = {inputs => $inputs, outputs => $outputs, args => $args};

    print Dumper($self);

    bless $self, $class;
}


sub inputs {
    return shift->{inputs};
}


sub outputs {
    return shift->{outputs};
}


sub args {
    return shift->{args};
}


1;

