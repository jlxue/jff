#!/usr/bin/perl -w
use strict;
use warnings;
use Archive::Zip qw/:ERROR_CODES :CONSTANTS/;
use CGI::Fast;
use CGI::Carp;
use File::Copy;
use File::Path;
use File::Spec;
use Digest;
use HTML::Template;
use POSIX;

use constant {
    E_WRONG_FILE_TYPE       => "Wrong file type, only .swf and .zip allowed!",
    E_CGI_ERROR             => "Failed to upload this file!",
    E_ALREADY_EXIST         => "This file has been uploaded by somebody!",
    E_INVALID_NAME          => "Invalid name, it's length must be less or equal to 32!",
    E_INVALID_DIMENSION     => "Invalid width or height, must be greater than 0 and less or equal to 400!",
    E_NO_CONTENT_FILE       => "You must upload a swf or zipped file!",
    E_NO_ICON_FILE          => "You must upload a icon file!",
    E_GENERATE_CONFIG       => "Failed to generate config.xml.",
    E_GENERATE_WGT          => "Failed to genrate .wgt file.",
    E_WRONG_ENTRY           => "Not specified entry file or entry file doesn't exist in the zipped file.",
};

use constant HTML_TEMPLATE_PATH => '/home/dieken/public_html';
use constant WIDGET_ROOT_DIR => '/home/dieken/public_html/widget/';
use constant WIDGET_BASE_URL => 'widget/';

my $tmpl_result = new HTML::Template(filename => 'result.tmpl',
                                     force_untaint => 0,
                                     cache => 1,
                                     loop_context_vars => 1,
                                     path => [HTML_TEMPLATE_PATH],
                                    );

my $tmpl_error = new HTML::Template(filename => 'error.tmpl',
                                    force_untaint => 0,
                                    cache => 1,
                                    loop_context_vars => 1,
                                    path => [HTML_TEMPLATE_PATH],
                                   );

$ENV{PATH} = '/bin:/usr/bin';

# $Module::Load::Conditional::FIND_VERSION = 0;       # To avoid tainted problem


while (my $q = new CGI::Fast) {
    my ($req, $error, $tmpl);
    my ($dir, $subdir, $unpacked_dir, $wgt_hexdigest, $url);

    $error = parse_request($q, $req);
    goto L_ERROR if defined $error;

    $error = prepare_dest_directory(WIDGET_ROOT_DIR, $req->{'content_hexdigest'},
                                    $dir, $subdir, $unpacked_dir);
    goto L_ERROR if defined $error;

    $error = move_or_unpack_uploaded_files($req->{'content_filename'}, $req->{'content_tmpFileName'},
                                           $req->{'icon_filename'}, $req->{'icon_tmpFileName'},
                                           $req->{'content_entry'}, $unpacked_dir);
    goto L_ERROR if defined $error;

    $error = generate_widget_config_xml(File::Spec->catfile($unpacked_dir, 'config.xml'), $req);
    goto L_ERROR if defined $error;


    $error = pack_widget($unpacked_dir, $subdir, $wgt_hexdigest);
    goto L_ERROR if defined $error;


    ### render html page
    $tmpl = $tmpl_result;
    $url = WIDGET_BASE_URL . substr($subdir, length(WIDGET_ROOT_DIR)) .  '/' . $wgt_hexdigest . '.wgt';
    $tmpl->param(filename => $req->{'content_filename'}, digest => $req->{'content_hexdigest'},
                 url => $url, wgt_digest => $wgt_hexdigest);
    goto L_OUTPUT;

L_ERROR:
    $tmpl = $tmpl_error;
    if ($error eq E_ALREADY_EXIST) {
        $tmpl->param(errormsg => $req->{'content_filename'} . " (" . $req->{'content_hexdigest'} . "): $error");
    } else {
        rmtree($subdir);
        rmdir($dir);
        $tmpl->param(errormsg => $error);
    }

L_OUTPUT:
    print $q->header(-type => "text/html; charset=UTF-8");
    $tmpl->output(print_to => select());
}


############################################################
sub parse_request {
    my $q = $_[0];

    my %req= ();
    my $digest = new Digest('MD5');

    $req{'content_filename'}    = $q->param('file');
    $req{'content_tmpFileName'} = $q->tmpFileName($req{'content_filename'});
    $req{'icon_filename'}       = $q->param('icon');
    $req{'icon_tmpFileName'}    = $q->tmpFileName($req{'icon_filename'});

    return E_CGI_ERROR if $q->cgi_error;
    return E_NO_CONTENT_FILE if ! $q->upload('file');
    return E_NO_ICON_FILE if ! $q->upload('icon');

    if ($req{'content_filename'} !~ /\.(?:zip|swf)$/i || $req{'icon_filename'} !~ /\.(?:gif|jpg|png)$/i) {
        return E_WRONG_FILE_TYPE;
    }


    if ($req{'content_filename'} =~ /\.zip$/) {
        $req{'content_entry'}   = $q->param('entry') || '';
        $req{'content_entry'} =~ s/^[\\\/]+//;
        $req{'content_entry'} =~ s/\\/\//g;
        return E_WRONG_ENTRY if length($req{'content_entry'}) == 0;
    } else {
        $req{'content_entry'}   = $req{'content_filename'};
    }


    $digest->addfile($q->upload('file'));
    $req{'content_hexdigest'}   = $digest->hexdigest;

    $req{'width'}               = $q->param('width') || 100;
    $req{'height'}              = $q->param('height') || 100;
    $req{'network_enabled'}     = $q->param('network_enabled') || 'false';
    $req{'name'}                = $q->param('name') || '';
    $req{'description'}         = $q->param('description') || '';
    $req{'author_name'}         = $q->param('author_name') || 'Unknown';
    $req{'author_url'}          = $q->param('author_url') || 'http://unknown.site/';
    $req{'author_email'}        = $q->param('author_email') || 'unknown@some.site';
    $req{'license'}             = $q->param('license') || '';

    if (length($req{'name'}) == 0 || length($req{'name'}) > 32) {
        return E_INVALID_NAME;
    }

    if ($req{'width'} !~ /^[1-9]\d*$/ || $req{'height'} !~ /^[1-9]\d*$/ ||
            $req{'width'} > 400 || $req{'height'} > 400) {
        return E_INVALID_DIMENSION;
    }

    $_[1] = \%req;
    return undef;
}


sub prepare_dest_directory {
    my ($root, $hexdigest) = @_;
    my ($dir, $subdir, $unpacked_dir);

    $dir = File::Spec->catdir($root, substr($hexdigest, 0, 2));
    $subdir = File::Spec->catdir($dir, substr($hexdigest, 2));
    $unpacked_dir = File::Spec->catdir($subdir, 'unpacked');

    if (! mkdir($dir) && ($! + 0) != POSIX::EEXIST) {
        return E_CGI_ERROR;
    }

    if (! mkdir($subdir)) {
        if (($! + 0) != POSIX::EEXIST) {
            return E_CGI_ERROR;
        } else {
            return E_ALREADY_EXIST;
        }
    }

    if (! mkdir($unpacked_dir)) {
        return E_CGI_ERROR;
    }

    $_[2] = $dir;
    $_[3] = $subdir;
    $_[4] = $unpacked_dir;
    return undef;
}


sub move_or_unpack_uploaded_files {
    my ($content_filename, $content_tmpFileName,
        $icon_filename, $icon_tmpFileName,
        $content_entry, $unpacked_dir) = @_;

    if ($content_filename =~ /zip$/) {
        my $zip = new Archive::Zip;
        $zip->read($content_tmpFileName) == AZ_OK or return E_CGI_ERROR;

        ### deal with this case gracefully:
        ### aa.zip/
        #       aa/a.swf
        #       aa/b.swf
        #   $content_entry is a.swf, I want to strip prefix "aa/" and
        #   make sure $content_entry exist.
        my @members = $zip->memberNames();
        my $found = 0;
        my $entry;
        for (@members) {
            if (rindex($_, $content_entry) >= 0 &&
                    rindex($_, $content_entry) == length($_) - length($content_entry)) {
                ++$found;
                $entry = $_;
            }
        }
        return E_WRONG_ENTRY if $found != 1;

        ### deduce prefix according $entry (full name in zipped file) and
        ### $content_entry (relative path specified by user).
        my $prefix = substr($entry, 0, rindex($entry, $content_entry));
        ### make sure all files are started with this prefix.
        for (@members) {
            return E_WRONG_ENTRY if index($_, $prefix) != 0;
        }

        if ($unpacked_dir !~ /\/$/) {
            $unpacked_dir .= '/';
        }
        $zip->extractTree($prefix, $unpacked_dir) == AZ_OK or return E_CGI_ERROR;
    } else {
        copy($content_tmpFileName, File::Spec->catfile($unpacked_dir, $content_filename))
            or return E_CGI_ERROR;
    }

    copy($icon_tmpFileName, File::Spec->catfile($unpacked_dir, $icon_filename))
        or return E_CGI_ERROR;

    return undef;
}


sub generate_widget_config_xml {
    my ($file, $req) = @_;
    my $hexdigest   = $req->{'content_hexdigest'};
    my $width       = $req->{'width'};
    my $height      = $req->{'height'};
    my $name        = CGI::escapeHTML($req->{'name'});
    my $description = CGI::escapeHTML($req->{'description'});
    my $url         = CGI::escapeHTML($req->{'author_url'});
    my $email       = CGI::escapeHTML($req->{'author_email'});
    my $author_name = CGI::escapeHTML($req->{'author_name'});
    my $icon        = CGI::escapeHTML($req->{'icon_filename'});
    my $network     = CGI::escapeHTML($req->{'network_enabled'});
    my $license     = CGI::escapeHTML($req->{'license'});
    my $entry       = CGI::escapeHTML($req->{'content_entry'});
    my $type;

    if ($entry =~ /\.swf/i) {
        $type = "application/x-shockwave-flash";
    } else {
        $type = "text/html";
    }

    open my $fh, '>', $file or return E_GENERATE_CONFIG;
    print $fh <<END;
<?xml version="1.0" encoding="utf-8"?>
<widget xmlns="http://www.w3.org/ns/widgets" id="http://nfbw/$hexdigest"
        version="1.1.0" width="$width" height="$height">
    <name>$name</name>
    <description>$description</description>
    <author url="$url" email="$email">$author_name</author>
    <icon src="$icon" />
    <access network="$network" />
    <license>$license</license>  
    <content src="$entry" type="$type" />
</widget>
END
    close $fh;
    return undef;
}


sub pack_widget {
    my ($unpacked_dir, $subdir) = @_;
    
    my $zip = new Archive::Zip;
    $zip->addTree($unpacked_dir) == AZ_OK or return E_GENERATE_WGT;

    my $tmpfilename = File::Spec->catfile($subdir, 'widget.wgt');
    return E_GENERATE_WGT if AZ_OK != $zip->writeToFileNamed($tmpfilename);

    my $digest = new Digest('MD5');
    open my $fh, $tmpfilename or return E_GENERATE_WGT;
    binmode $fh;
    $digest->addfile($fh);
    close $fh;

    my $hexdigest = $digest->hexdigest;
    rename $tmpfilename, File::Spec->catfile($subdir, $hexdigest .  '.wgt')
        or return E_GENERATE_WGT;

    $_[2] = $hexdigest;

    return undef;
}

