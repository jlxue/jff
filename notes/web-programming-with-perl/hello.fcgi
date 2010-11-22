#!/usr/bin/perl
use strict;
use warnings;
use CGI::Fast;
use Cwd;
use File::Basename;
use HTML::Template::Compiled speed => 1;

while (my $q = CGI::Fast->new()) {
    $ENV{HTML_TEMPLATE_ROOT} = dirname($ENV{SCRIPT_FILENAME})
            unless exists $ENV{HTML_TEMPLATE_ROOT};
    my $template = HTML::Template::Compiled->new(
            filename => "hello.tmpl", cache => 1, loop_context_vars => 1);

    my $name = $q->param("name");
    $name = "Unknown" unless defined $name;
    my $user_info = "uid=$<(" . getpwuid($<) .  ") euid=$>(" .  getpwuid($>) .
            ") gid=$((" . getgrgid($() . ") egid=$)(" .  getgrgid($)) . ")";

    $template->param(name => $name, incs => "@INC", envs => \%ENV,
                     cwd => getcwd(), user_info => $user_info);
    my $content = $template->output();

    print $q->header(-type => "text/html", -Content_length => length($content)),
          $content;
}
