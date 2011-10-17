package Event::Plumber;
use strict;
use warnings;
use HTTP::Request;
use JSON;
use constant    DEFAULT_PORT    => 9999;

sub new {
    my ($class, %args) = @_;

    if (! exists $args{host}) {
        $args{host} = "localhost";
    }

    if (! $args{port}) {
        if (exists $ENV{PLUMBER_PORT}) {
            $args{port} = $ENV{PLUMBER_PORT};
        } else {
            $args{port} = DEFAULT_PORT;
        }
    }

    bless \%args, $class;
}


# Args:     action, args...
# Returns:  HTTP::Request object
#
sub createRequest {
    my $self = shift;

    die "No action name specified!\n" unless @_ > 0;

    my $json = encode_json [@_];
    die "Failed to encode to JSON!\n" unless $json;

    return HTTP::Request->new(
        POST    => "http://$self->{host}:$self->{port}/json",
        [
            "Content-Type"  => "application/json",
        ],
        $json
    );
}


# Args:     HTTP::Response object
# Returns:  ["bad" | "ok", args....]
#
sub parseResponse {
    my $self = shift;
    my $response = shift;

    unless (defined $response) {
        return ["bad", "failed to obtain HTTP response!"];
    }

    die "Not a HTTP::Response object!\n" unless
        ref($response) && $response->isa('HTTP::Response');

    my $content = $response->content;

    if ($response->code != 200) {
        return ["bad", defined($content) ? $content : "Empty response!"];
    }

    return decode_json($content);
}


# Args:     "app_name", args...
# Returns:  HTTP::Request object
#
sub createExecRequest {
    my $self = shift;

    die "No app name specified!\n" unless @_ > 0;

    return $self->createRequest("exec", @_);
}


# Args:     args...
# Returns:  HTTP::Request object
#
sub createEstablishRequest {
    my $self = shift;

    die "Not local plumber!" if $self->{host} ne "localhost";
    die "No cookie found!" unless $ENV{PLUMBER_COOKIE};

    return $self->createRequest("establish", $ENV{PLUMBER_COOKIE}, @_);
}


# Args:     args...
# Returns:  HTTP::Request object
#
sub createEstablish2Request {
    my $self = shift;

    die "Not local plumber!" if $self->{host} ne "localhost";
    die "No cookie found!" unless $ENV{PLUMBER_COOKIE};

    return $self->createRequest("establish2", $ENV{PLUMBER_COOKIE}, @_);
}


1;

