#!/usr/bin/perl
use strict;
use warnings;
use Apache2::RequestUtil;
use Cwd;
use File::Basename;
use HTML::Template::Compiled speed => 1;

$ENV{HTML_TEMPLATE_ROOT} = dirname($ENV{SCRIPT_FILENAME}) unless exists $ENV{HTML_TEMPLATE_ROOT};
my $template = HTML::Template::Compiled->new(
        filename => "hello.tmpl", cache => 1, loop_context_vars => 1);

my $r = Apache2::RequestUtil->request;
my $name = $r->args();
($name) = $name =~ /name=([^&]*)/;
$name = "Unknown" unless defined $name;
my $user_info = "uid=$<(" . getpwuid($<) .  ") euid=$>(" .  getpwuid($>) .
        ") gid=$((" . getgrgid($() . ") egid=$)(" .  getgrgid($)) . ")";

$template->param(name => $name, incs => "@INC", envs => \%ENV,
                 cwd => getcwd(), user_info => $user_info);
my $content = $template->output();

$r->content_type("text/html");
$r->print($content);
