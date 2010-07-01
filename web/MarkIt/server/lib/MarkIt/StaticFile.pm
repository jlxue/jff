package MarkIt::StaticFile;
use Cwd qw/abs_path/;
use FileHandle   ();
use File::MMagic ();
use File::Spec::Functions qw(canonpath);
use MIME::Types  ();
use URI::Escape  ();
use base 'MarkIt::Base';

use constant ROOT   => abs_path('t/www');

our $VERSION = '0.01';

my $mime  = MIME::Types->new();
my $magic = File::MMagic->new();


sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/view/]);
}

sub view {
    my ($c) = @_;

    my $path = $c->param('path');
    $c->log->info("request static file: [$path]\n");

    if (! defined($path) || $path eq '/' || length($path) == 0) {
        $path = ROOT . '/index.html';
    } else {
        my $p = abs_path(ROOT . '/' . canonpath(URI::Escape::uri_unescape($path)));
        if (index($p, ROOT) != 0) {
            $c->error(title => "Stop!", msg => "Can't access " . $p);
            return;
        }

        $path = $p;
    }

    $c->log->info("response static file: [$path]\n");

    if (my $content = $c->send_file($path)) {
        return $content;
    } else {
        return $self->error_mode();
    }
}

# modified from CGI::Application::Plugin::Stream::stream_file() and
# HTTP::Server::Simple::Static::serve_static().
sub send_file {
    my ($c, $path) = @_;
    my ($fh, $size, $mime_type, $output);

    open $fh, $path || return 0;
    binmode $fh;

    # Content-Length
    #
    $size = (stat $fh)[7];
    $c->header_add(-Content_Length => $size);

    # Content-Type
    #
    my $mimeobj = $mime->mimeTypeOf($path);
    if (defined $mimeobj) {
        $mime_type = $mimeobj->type;
        $c->log->debug("MIME::Types got: $mime_type\n");
    } else {
        bless $fh, "FileHandle";    # make File:MMagic happy
        $mime_type = $magic->checktype_filehandle($fh);

        if ($mime_type) {
            $c->log->debug("File::Magic got: $mime_type\n");
        } else {
            $mime_type = 'application/octet-stream';
        }

        seek($fh, 0, 0);
    }
    $c->header_add(-type => $mime_type);

    # output header if necessary
    unless ($ENV{CGI_APP_RETURN_ONLY}) {
        $c->header_type('none');
        print $c->query->header($c->header_props());
    }

    # body
    if ($ENV{CGI_APP_RETURN_ONLY}) {
        local $/;
        $output = <$fh>;
    } else {
        while (read($fh, my $buffer, 1024)) {
            print $buffer;
        }
        print '';   # print a null string at the end
    }

    close $fh;

    return $ENV{CGI_APP_RETURN_ONLY} ? \$output : 1;
}

1;

