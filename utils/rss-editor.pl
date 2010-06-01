#!/usr/bin/perl
#
# Purpose: a simple RSS editor written with Perl/Tk
#
# Usage:
#   perl rss-editor.pl [some.rss]
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2010-05-28      Liu Yubao
#       * initial version, v0.1
#       * support up/down button
#       * fix bad font on MS Windows
#       * fix garbled pubDate on MS Windows
#       * change to XML::RSS::LibXML
#       * release v0.2
#
#   2010-05-29      Liu Yubao
#       * change back to XML::RSS because XML::RSS::LibXML doesn't support "encode_cb" options
#       * import Tk::FontDialog on demand
#       * fix undefined height problem in Cygwin
#       * release v0.2.1
#
# TODO:
#   * auto scroll to last TextUndo widget
#   * auto expand to fill whole main window,
#   * chinese input bar flash on MS Windows
#   * keep input method when switch to another TextUndo widget,

use DateTime;
use DateTime::Format::Mail;
use Encode;
use File::Basename;
use File::Spec;
use POSIX;
use Tk;
#use Tk::FileDialog;
use XML::RSS;
use strict;
use warnings;

use constant CHANNEL_ATTRS   => qw(
    category
    copyright
    description
    docs
    generator
    language
    lastBuildDate
    link
    managingEditor
    pubDate
    rating
    title
    ttl
    webMaster
);

use constant IMAGE_ATTRS     => qw(
    description
    height
    link
    title
    url
    width
);

use constant TEXTINPUT_ATTRS => qw(
    description
    link
    name
    title
);

use constant ITEM_ATTRS      => qw(
    author
    category
    comments
    description
    guid
    link
    pubDate
    title
);
    #source
    #enclosure


our $VERSION = '0.2.1';

my $rss = XML::RSS->new (version => '2.0',
    encode_output => 1, encode_cb => \&encode_rss);

Tk::CmdLine::LoadResources();
Tk::CmdLine::SetArguments();

my $mw = new MainWindow();

#my $filedialog = $mw->FileDialog();
my $fontdialog;
my $file = '';                  # rss 文件路径
my %rss_widgets = ();           # type => Tk::TextUndo, type 取值 channel, image, textinput, item-NNN
                                # save/load rss 时根据 type 更新 $rss 或者 Tk::TextUndo
my @item_widgets = ();          # LabFrame 数组，保存每个 item 所在的控件，每个 LabFrame 的 label 是 item-NNN

my $scrolled = $mw->Scrolled(
    qw/Frame
    -width          800
    -height         600
    -scrollbars     osoe
    /)->pack(qw/-expand 1 -fill both/);

add_widget($scrolled, 'channel', CHANNEL_ATTRS);
add_widget($scrolled, 'image', IMAGE_ATTRS);
add_widget($scrolled, 'textinput', TEXTINPUT_ATTRS);

$mw->Label(-textvariable => \$file)->pack();

my $toolbar = $mw->Frame()->pack();
$toolbar->Button(-text => 'Load', -command => \&load)->pack(-side => 'left');
$toolbar->Button(-text => 'Save', -command => \&save)->pack(-side => 'left');
$toolbar->Button(-text => 'Save as...', -command => \&saveas)->pack(-side => 'left');
$toolbar->Button(-text => 'New item', -command => sub {
        my $w = add_widget($scrolled, 'item-' . next_item_id(), ITEM_ATTRS);
    })->pack(-side => 'left');
$toolbar->Button(-text => 'Select font...', -command => sub {
        if (! defined $fontdialog) {
            require Tk::FontDialog;
            Tk::FontDialog->import;

            $fontdialog = $mw->FontDialog(
                -sampletext => decode_utf8("The Quick Brown Fox Jumps Over The Lazy Dog\n中华人民共和国万岁！"),
            );
        }

        my $font = $fontdialog->Show;
        if (defined $font) {
            my $fontname = $mw->GetDescriptiveFontName($font);
            print "Select font: ", ($^O eq 'MSWin32' ? encode('GBK', $fontname) : $fontname), "\n";
            $mw->RefontTree(-font => $font);
        }
    })->pack(-side => 'left');

if (@ARGV > 0 && -f $ARGV[0]) {
    $file = File::Spec->rel2abs($ARGV[0]);
    loadrss();
}

MainLoop;

sub add_widget {
    my ($scrolled, $type, @attrs) = @_;

    my $labframe = $scrolled->LabFrame(
        -label      => $type,
    )->pack(-fill => 'x');

    if ($type =~ /^item/) {
        push @item_widgets, $labframe;

        my $w = $labframe->Frame()->pack(-fill => 'x');
        $w->Button(
            -text => 'Up',
            -command => sub {
                move_item_widget($labframe, -1);
            },
        )->pack(-side => 'left');

        $w->Button(
            -text => 'Down',
            -command => sub {
                move_item_widget($labframe, 1);
            },
        )->pack(-side => 'left');

        $w->Button(
            -text => 'Delete',
            -command => sub {
                delete_item_widget($labframe);
            },
        )->pack(-side => 'left');
    }

    for my $attr (@attrs) {
        my $name = "$type $attr";

        my $frame = $labframe->Frame()->pack(-fill => 'x');

        $frame->Label(
            -text       => $name,
            -width      => 20,
            -anchor     => 'w',
        )->pack(-side => 'left');

        $rss_widgets{$name} = $frame->Scrolled(
            'TextUndo',
            -height     => 3,
            -scrollbars => 'osoe'
        )->pack(-side => 'left', -fill => 'x');

        $rss_widgets{$name}->configure(-font => ["NSimSun", -16]) if $^O eq 'MSWin32';

        if ($attr eq 'pubDate' or $attr eq 'lastBuildDate') {
            $rss_widgets{$name}->Contents(current_datetime());
        } elsif ($attr eq 'language') {
            $rss_widgets{$name}->Contents('zh-cn');
        } elsif ($attr eq 'generator') {
            $rss_widgets{$name}->Contents("rss-editor.pl v$VERSION");
        } elsif ($attr eq 'ttl') {
            $rss_widgets{$name}->Contents("5");
        } elsif ($attr eq 'managingEditor') {
            $rss_widgets{$name}->Contents('editor@example.com');
        } elsif ($attr eq 'webMaster') {
            $rss_widgets{$name}->Contents('webmaster@example.com');
        } elsif ($attr eq 'width') {
            $rss_widgets{$name}->Contents('88');    # <=144
        } elsif ($attr eq 'height') {
            $rss_widgets{$name}->Contents('31');    # <=400
        } elsif ($attr eq 'author') {
            $rss_widgets{$name}->Contents('author@example.com');
        }
    }

    return $labframe;
}

sub load {
    my ($dirname, $basename);
    ($dirname, $basename) = (dirname $file, basename $file) if length($file) > 0;

    my @options = ();
    push @options, -initialdir => $dirname, -initialfile => $basename if defined $basename;
    my $f = $mw->getOpenFile(
        @options,
        #-filetypes => ['RSS Files', ['.xml', '.rss', '.rdf']],
    );

    #$filedialog->configure(-Create => 0);
    #if (defined $basename) {
    #    $filedialog->configure(-File => $basename);
    #    $filedialog->configure(-Path => $dirname);
    #}
    #my $f = $filedialog->Show();

    return if ! defined $f;
    $file = $f;

    loadrss();
}

sub save {
    if (length($file) == 0) {
        saveas($_[0]);
    } else {
        saverss();
    }
}

sub saveas {
    my ($dirname, $basename);
    ($dirname, $basename) = (dirname $file, basename $file) if length($file) > 0;

    my @options = ();
    push @options, -initialdir => $dirname, -initialfile => $basename if defined $basename;
    my $f = $mw->getSaveFile(
        @options,
        #-defaultextension => '.xml',
        #-filetypes => ['RSS Files', ['.xml', '.rss', '.rdf']],
    );

    #$filedialog->configure(-Create => 1);
    #if (defined $basename) {
    #    $filedialog->configure(-File => $basename);
    #    $filedialog->configure(-Path => $dirname);
    #}
    #my $f = $filedialog->Show();

    return if ! defined $f;
    $file = $f;

    saverss();
}

sub loadrss {
    $rss->parsefile($file);

    print "\n", "#" x 70, "\n";
    print $rss->as_string;
    print "\n", "#" x 70, "\n";

    load_rss_element('channel', CHANNEL_ATTRS);
    load_rss_element('image', IMAGE_ATTRS);
    load_rss_element('textinput', TEXTINPUT_ATTRS);

    for (@item_widgets) {
        $_->destroy();
    }
    @item_widgets = ();

    my @items = @{$rss->{items}};
    my $i = 1;
    for my $item (@items) {
        add_widget($scrolled, "item-$i", ITEM_ATTRS);
        load_rss_element("item-$i", ITEM_ATTRS);
        ++$i;
    }
}

sub load_rss_element {
    my ($type, @attrs) = @_;

    for my $attr (@attrs) {
        my $value;
        if ($type =~ /^item-(\d+)/) {
            my $item = $rss->{items}->[$1 - 1];
            next if ! defined $item;
            $value = $item->{$attr};
        } else {
            $value = $rss->$type($attr);
        }
        next if ! defined $value;

        $value =~ s/^\s+|\s+$//g;
        $rss_widgets{"$type $attr"}->Contents($value);
    }
}

sub saverss {
    save_rss_element('channel', CHANNEL_ATTRS);
    save_rss_element('image', IMAGE_ATTRS);
    save_rss_element('textinput', TEXTINPUT_ATTRS);

    $rss->{'items'} = [];
    for my $w (@item_widgets) {
        my $type = $w->cget('-label');
        save_rss_element($type, ITEM_ATTRS);
    }

    $rss->save($file);
}

sub save_rss_element {
    my ($type, @attrs) = @_;
    my %h = ();

    for my $attr (@attrs) {
        my $value = $rss_widgets{"$type $attr"}->Contents();
        $value =~ s/^\s+|\s+$//g;
        $h{$attr} = $value if length($value) > 0;
    }

    if ($type eq 'channel') {
        $h{lastBuildDate} = current_datetime();
    }

    if ($type =~ /^item/) {
        $rss->add_item(%h);
    } else {
        $rss->$type(%h);
    }
}

sub encode_rss {
    my ($obj, $text) = @_;
    $text =~ s/^\s+|\s+$//g;
    while (chomp $text) {
    }
    return '<![CDATA[' . $text . ']]>';
}

sub next_item_id {
    my $id = 0;

    for my $w (@item_widgets) {
        my $type = $w->cget('-label');
        if ($type =~ /^item-(\d+)/) {
            $id = $1 if $id < $1;
        }
    }

    return $id + 1;
}

sub move_item_widget {
    my ($w, $direction) = @_;

    my $i = find_item_widget($w);
    if ($direction < 0) {   # up
        return if $i == 0;
        $w->pack(-before => $item_widgets[$i + $direction]);
    } else {                # down
        return if $i == $#item_widgets;
        $w->pack(-after => $item_widgets[$i + $direction]);
    }

    $item_widgets[$i] = $item_widgets[$i + $direction];
    $item_widgets[$i + $direction] = $w;
}

sub delete_item_widget {
    my ($w) = @_;

    $w->destroy;
    my $i = find_item_widget($w);
    splice @item_widgets, $i, 1;
}

sub find_item_widget {
    my ($w) = @_;

    for (my $i = 0; $i < @item_widgets; ++$i) {
        return $i if $w == $item_widgets[$i];
    }
}

sub current_datetime {
    DateTime::Format::Mail->format_datetime(DateTime->now);
}

