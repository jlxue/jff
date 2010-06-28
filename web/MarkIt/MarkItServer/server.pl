use warnings;
use strict;
use CGI::Application::Server;
use lib 'lib';
use MarkItServer;

my $app = MarkItServer->new(PARAMS => {

});

my $server = CGI::Application::Server->new();
$server->document_root('./t/www');
$server->entry_points({
    '/index.cgi' => $app,
});
$server->run;
