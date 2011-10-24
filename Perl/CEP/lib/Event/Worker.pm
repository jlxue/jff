package Event::Worker;
use strict;
use warnings;
use Data::Dumper;
use Event::Plumber;
use JSON;
use LWP::UserAgent;
use Module::Load;
use POE qw(Component::Server::TCP Component::Client::TCP);
use Socket qw/unpack_sockaddr_in inet_ntoa/;

sub new {
    my ($class, %params) = @_;

    my $plumber = Event::Plumber->new();
    my @response = $plumber->establish();

    die $response[1] if $response[0] ne "ok";

    my $h = $response[1];

    my $self = {};
    $self->{inputs} = $h->{inputs} || [];
    $self->{outputs} = $h->{outputs} || {};
    $self->{args} = $h->{args} || [];

    $self->{input_poe_session_ids} = {};
    $self->{output_poe_session_ids} = {};

    $self->{onInput} =  $params{onInput} || \&onInput;
    $self->{onFeedback} = $params{onFeedback} || \&onFeedback;

    $self->{HighMark} = $params{HighMark} || 10 * 1024 * 1024;

    $self->{InputFilter} = $params{InputFilter};
    $self->{OutputFilter} = $params{OutputFilter};

    $self->{FeedbackInputFilter} = $params{FeedbackInputFilter};
    $self->{FeedbackOutputFilter} = $params{FeedbackOutputFilter};

    bless $self, $class;
}


sub inputs {
    return @{ shift->{inputs} };
}


sub outputs {
    return keys %{ shift->{outputs} };
}


sub args {
    return shift->{args};
}


sub run {
    my $self = shift;
    my $outputs = $self->{outputs};

    if (keys %$outputs > 0) {
        $self->_connectOutputs();
    } else {
        $self->_connectInputs();
    }

    POE::Kernel->run();
}


# See POE::Component::Server::TCP  ClientFilter
#
sub _createFilter {
    my $filter = shift;

    my $ref_type = ref($filter);
    if ($ref_type eq "") {
        load $filter;
        return $filter->new;
    } elsif ($ref_type eq 'ARRAY') {
        die "Bad filter argument!" if @$filter < 1;

        my $class = shift @$filter;
        load $class;
        return $class->new(@$filter);
    } elsif ($filter->isa("POE::Filter")) {
        return $filter->clone();
    } else {
        die "Bad filter argument!";
    }
}


sub _connectOutputs {
    my $self = shift;
    my $outputs = $self->{outputs};

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

                $_[HEAP]{server}->pause_input();

                if ($line =~ /^ok\b/i) {
                    $self->{output_poe_session_ids}{$output} = $_[SESSION]->ID;

                    $_[KERNEL]->state("ServerInput", sub {
                            $self->{onFeedback}->($output, $_[ARG0]);
                        });

                    $_[HEAP]{server}->set_high_mark($self->{HighMark});

                    if ($self->{OutputFilter}) {
                        $_[HEAP]{server}->set_output_filter(_createFilter($self->{OutputFilter}));
                    }

                    if ($self->{FeedbackInputFilter}) {
                        $_[HEAP]{server}->set_input_filter(_createFilter($self->{FeedbackInputFilter}));
                    }

                } else {
                    $_[KERNEL]->state("ServerInput");

                    die "failed to connect output \"$output\": $line";
                }

                if (keys %{ $self->{output_poe_session_ids} } == keys %{ $self->{outputs} }) {
                    print "connected all outputs\n";

                    for my $sid (values %{ $self->{output_poe_session_ids} }) {
                        my $session = $poe_kernel->ID_id_to_session($sid);
                        die "Bad session!" unless defined $session;
                        $session->get_heap()->{server}->resume_input();
                    }

                    $self->_connectInputs();
                }
            }
        );
    }
}

sub _connectInputs {
    my $self = shift;
    my $inputs = $self->{inputs};

    if (@$inputs == 0) {
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

            $_[KERNEL]->state("ClientInput");
            $_[HEAP]{client}->pause_input();

            my ($secret2, $input) = split / /, $line;

            if ($secret ne $secret2) {
                $_[HEAP]{client}->put("bad Wrong secret");
                $_[KERNEL]->yield("shutdown");

            } elsif (exists $self->{input_poe_session_ids}{$input}) {
                $_[HEAP]{client}->put("bad Duplicate input $input");
                $_[KERNEL]->yield("shutdown");

            } elsif (! grep {$_ eq $input} @{ $self->{inputs} }) {
                $_[HEAP]{client}->put("bad Unknown input $input");
                $_[KERNEL]->yield("shutdown");

            } else {
                $self->{input_poe_session_ids}{$input} = $_[SESSION]->ID;
                $_[KERNEL]->state("ClientInput", sub {
                        $self->{onInput}->($input, $_[ARG0]);
                    });

                $_[HEAP]{client}->put("ok");

                $_[HEAP]{client}->set_high_mark($self->{HighMark});

                if ($self->{InputFilter}) {
                    $_[HEAP]{client}->set_input_filter(_createFilter($self->{InputFilter}));
                }

                if ($self->{FeedbackOutputFilter}) {
                    $_[HEAP]{client}->set_output_filter(_createFilter($self->{FeedbackOutputFilter}));
                }

                # Don't listen again if inputs are all connected.
                if (keys %{ $self->{input_poe_session_ids} } == @{ $self->{inputs} }) {
                    for my $sid (values %{ $self->{input_poe_session_ids} }) {
                        my $session = $poe_kernel->ID_id_to_session($sid);
                        die "Bad session!" unless defined $session;
                        $session->get_heap()->{client}->resume_input();
                    }

                    $_[KERNEL]->post("Listener", "shutdown");
                }
            }
        }
    );
}


sub onInput {
    my ($input_name, $data) = @_;

    print "pid $$ got input from $input_name: $data\n";
}


sub onFeedback {
    my ($output_name, $data) = @_;

    print "pid $$ got feedback from $output_name: $data\n";
}


sub output {
    my ($self, $output_name, $data) = @_;

    die "Unknown output: $output_name" unless
        exists $self->{output_poe_session_ids}{$output_name};

    my $sid = $self->{output_poe_session_ids}{$output_name};
    my $session = $poe_kernel->ID_id_to_session($sid);

    die "POE session not found!" unless defined $session;

    $session->get_heap()->{server}->put($data);
}


sub feedback {
    my ($self, $input_name, $data) = @_;

    die "Unknown input: $input_name" unless
        exists $self->{input_poe_session_ids}{$input_name};

    my $sid = $self->{input_poe_session_ids}{$input_name};
    my $session = $poe_kernel->ID_id_to_session($sid);

    die "POE session not found!" unless defined $session;

    $session->get_heap()->{client}->put($data);
}


1;

