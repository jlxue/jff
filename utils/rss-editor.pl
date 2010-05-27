#!/usr/bin/perl
use Tk;
use XML::RSS;
use strict;
use warnings;

use constant CHANNEL_ATTRS  => qw/title link language description
    ttl category
    rating copyright pubDate lastBuildDate docs managingEditor
    webMaster/;
use constant IMAGE_ATTRS    => qw/title url link width height description/;
use constant TEXTINPUT_ATTRS    => qw/title description name link/;
use constant ITEM_ATTRS => qw/title category pubDate comments link description author/;

my $rss = XML::RSS->new (version => '2.0');
my $mw = new MainWindow();
my %rss_widgets = ();

my $scrolled = $mw->Scrolled(
    qw/Frame
    -width          800
    -height         600
    -scrollbars     ose
    /)->pack(qw/-expand 1 -fill both/);

add_widget($scrolled, 'channel', CHANNEL_ATTRS);
add_widget($scrolled, 'image', IMAGE_ATTRS);
add_widget($scrolled, 'textinput', TEXTINPUT_ATTRS);
add_widget($scrolled, 'item', ITEM_ATTRS);

MainLoop;

sub add_widget {
    my ($scrolled, $type, @attrs) = @_;

    for my $attr (@attrs) {
        my $name = "$type $attr";

        my $frame = $scrolled->Frame()->pack(
        );

        $frame->Label(
            -text       => $name,
            -width      => 30,
            -anchor     => 'w',
        )->pack(-side => 'left');

        $rss_widgets{$name} = $frame->Entry(
            -width      => 80,
        )->pack();
    }
}

