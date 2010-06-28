package MarkItServer;
use Dancer ':syntax';

our $VERSION = '0.1';


sub send_file_with_headers {
    my $file = shift;
    my $response = send_file $file;

    Dancer::Response::set($response);

    headers(@_);

    return $response;
}

get '/' => sub {
    send_file_with_headers 'index.html',
        'Access-Control-Allow-Origin'    => '*';
};

get '/markit.min.js' => sub {
    send_file_with_headers 'index.html',
        'Access-Control-Allow-Origin'    => '*';
};

get '/markit.js' => sub {
    send_file_with_headers 'index.html',
        'Access-Control-Allow-Origin'    => '*';
};

true;
