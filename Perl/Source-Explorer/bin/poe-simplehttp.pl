#!/usr/bin/perl
use POE qw(Filter::Block);
use POE::Component::Server::SimpleHTTP;
use POSIX;
use Sys::Hostname qw( hostname );
use Smart::Comments;
use strict;
use warnings;

# Start the server!
POE::Component::Server::SimpleHTTP->new(
    'ALIAS'     =>  'HTTPD',
    'ADDRESS'   =>  0,
    'PORT'      =>  8080,
    'KEEPALIVE' =>  1,
    'HANDLERS'  =>  [
        {
            'DIR'       =>  '.*',
            'SESSION'   =>  'HTTP_GET',
            'EVENT'     =>  'GOT_MAIN',
        },
    ],
    'HEADERS'   =>  {
        'Server'    =>  'My Own Server',
    },
) or die 'Unable to create the HTTP Server';

# Create our own session to receive events from SimpleHTTP
POE::Session->create(
    inline_states => {
        '_start'     =>  sub { $_[KERNEL]->alias_set( 'HTTP_GET' ) },
        'GOT_MAIN'   => \&GOT_REQ,
        'GOT_STREAM' => \&GOT_STREAM,
        'FILE_INPUT' => \&FILE_INPUT,
    },
);

# Start POE!
POE::Kernel->run();

# We're done!
exit;


sub GOT_REQ {
    # ARG0 = HTTP::Request object, ARG1 = HTTP::Response object, ARG2 = the DIR that matched
    my( $request, $response, $dirmatch ) = @_[ ARG0 .. ARG2 ];

    # Check for errors
    if ( ! defined $request ) {
        $_[KERNEL]->post( 'HTTPD', 'DONE', $response );
        return;
    }

    # Do our stuff to HTTP::Response
    $response->content_type("text/plain");

    open my $fh, $request->uri->path;
    binmode $fh;
    if (! defined $fh) {
        $response->code( 404 );
        $response->content($request->uri . "not readable!\n");
        $_[KERNEL]->post('HTTPD', 'DONE', $response);
        return;
    } else {
        $response->code( 200 );
    }

    # sets the response as streamed within our session with the stream event
    $response->stream(
        session  => 'HTTP_GET',
        event    => 'GOT_STREAM',
        dont_flush => 1,
    );

    my $wheel = new POE::Wheel::ReadWrite(
        Handle      => $fh,
        #Filter      => new POE::Filter::Block(BlockSize => POSIX::BUFSIZ * 4),
        Filter      => new POE::Filter::Block(BlockSize => 16),
        InputEvent  => "FILE_INPUT",
        ErrorEvent  => "FILE_ERROR",
    );

    ### $wheel
    $_[HEAP]->{wheel} = $wheel;
    $_[HEAP]->{response} = $response;
}


sub FILE_INPUT {
    my ($heap, $input, $wheel_id) = @_[HEAP, ARG0, ARG1];
    print STDERR "got file input, length=", length($input), "\n";
    print STDERR "[[[$input]]]\n";

    my $response = $_[HEAP]->{response};
    $response->content($input);
    $_[KERNEL]->post("HTTPD", "STREAM", $response);
}


sub FILE_ERROR {
    print STDERR "got file error!\n";
}


sub GOT_STREAM {
   my ( $kernel, $heap, $response ) = @_[KERNEL, HEAP, ARG0];

   # the stream hash contains the wheel, the request, the response
   # and an id associated the the wheel
   ### $response
   #$response->{'wheel'}->put("Hello World\n");
   #$response->content("Hello world" . time() . "\n");
   #$kernel->post("HTTPD", "STREAM", $response);
}

