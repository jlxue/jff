package Event::Worker;
use strict;
use warnings;
use Data::Dumper;
use Event::Plumber;
use JSON;
use LWP::UserAgent;
use POE qw(Component::Server::TCP Component::Client::TCP);
use Socket qw/unpack_sockaddr_in/;

sub new {
    my $class = shift;

    my $plumber = Event::Plumber->new();
    my @response = $plumber->establish();

    die $response[1] if $response[0] ne "ok";

    my $h = $response[1];

    my $self = {};
    $self->{inputs} = exists $h->{inputs} ? $h->{inputs} : [];
    $self->{outputs} = exists $h->{outputs} ? $h->{outputs} : {};
    $self->{args} = exists $h->{args} ? $h->{args} : [];

    $self->{input_poe_session_ids} = {};
    $self->{output_poe_session_ids} = {};

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


sub run {
    my $self = shift;
    my $outputs = $self->outputs;

    if (keys %$outputs > 0) {
        $self->_connectOutputs();
    } else {
        $self->_connectInputs();
    }

    POE::Kernel->run();
}


sub _connectOutputs {
    my $self = shift;
    my $outputs = $self->outputs;

    while (my ($output, $info) = each %$outputs) {
        die "Bad output information!" unless defined $info &&
            ref($info) eq 'ARRAY' && @$info == 3;

        my ($host, $port, $secret) = @$info;

        POE::Component::Client::TCP->new(
            RemoteAddress       => $host,
            RemotePort          => $port,

            Connected           => sub {
                $_[HEAP]{server}->put("$secret $output");
            },

            ServerInput         => sub {
                my $line = $_[ARG0];

                print "from server: $line\n";

                if ($line =~ /^ok\b/i) {
                    $self->{output_poe_session_ids}{$output} = $_[SESSION]->ID;
                    $_[HEAP]{name} = $output;

                    $_[KERNEL]->state("ServerInput", $self, "onChildInput");
                } else {
                    die "failed to connect output \"$output\": $line";
                }

                if (keys %{ $self->{output_poe_session_ids} } == keys %{ $self->{outputs} }) {
                    print "connected all outputs\n";

                    $self->_connectInputs();
                }
            }
        );
    }
}

sub _connectInputs {
    my $self = shift;
    my $inputs = $self->inputs;

    if (keys %$inputs == 0) {
        my $plumber = Event::Plumber->new();
        my @response = $plumber->establish2();

        die $response[1] if $response[0] ne "ok";
        return;
    }

    my ($host, $port, $secret);

    POE::Component::Server::TCP->new(
        Alias   => "Listener",

        Started => sub {
            my $listener = $_[HEAP]{listener};
            ($port, $host) = unpack_sockaddr_in($listener->getsockname);
            $host = inet_ntoa($host);
            $secret = "secret-$$";

            print "socket successfully bound to $host:$port\n";
            my $plumber = Event::Plumber->new();
            my @response = $plumber->establish2($port, $secret);
            die $response[1] if $response[0] ne "ok";
        },

        ClientConnected => sub {
            print "got a connection from $_[HEAP]{remote_ip} port ",
                $_[HEAP]{remote_port}, "\n";
        },

        ClientInput => sub {
            my $line = $_[ARG0];

            print "got a client input: $line\n";
            my ($secret2, $input) = split / /, $line;

            if ($secret ne $secret2) {
                $_[HEAP]{client}->put("bad Wrong secret");
                $_[KERNEL]->yield("shutdown");
            } elsif (exists $self->{input_poe_session_ids}{$input}) {
                $_[HEAP]{client}->put("bad Duplicate input $input");
                $_[KERNEL]->yield("shutdown");
            } else {
                $self->{input_poe_session_ids}{$input} = $_[SESSION]->ID;
                $_[HEAP]{client}->put("ok");

                # Don't listen again if inputs are all connected.
                if (keys %{ $self->{input_poe_session_ids} } == keys %{ $self->{inputs} }) {
                    $_[KERNEL]->post("Listener", "shutdown");
                }
            }
        }
    );
}

sub onChildInput {
}


sub onParentInput {
}

1;

