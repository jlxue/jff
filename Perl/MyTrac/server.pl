use warnings;
use strict;
use CGI::Application::Server;
use lib 'lib';
use MyTrac;

my $app = MyTrac->new(PARAMS => {
        "git_dir"   => "db/.git",
        "work_tree" => "db",
    });
my $server = CGI::Application::Server->new();

$server->document_root('./t/www');

$server->entry_points({
    '/mytrac'       => $app,
});

$server->run;

