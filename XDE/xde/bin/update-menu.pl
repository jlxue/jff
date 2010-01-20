#!/usr/bin/perl
use XML::SAX;
use strict;
use warnings;

BEGIN {
    MenuXMLHandler->import();
};

die "Usage: $0 filename\n" if @ARGV == 0;

XML::SAX::ParserFactory->parser(
    Handler => MenuXMLHandler->new())->parse_uri($ARGV[0]);
exit 0;

###############################################################
{
package MenuXMLHandler;
use base qw/XML::SAX::Base/;
use Smart::Comments;

my %element_handlers = (
    Menu        =>  \&handle_menu;
);

sub start_document {
    ### start_document: @_
}

sub end_document {
    ### end_document: @_
}

sub start_element {
    my ($self, $e) = @_;
    my $localname = $e->{LocalName};

    $self->{characters} = '';
    $element_handlers{$localname}->(@_) if exists $element_handlers{$localname};
}

sub end_element {
    ### end_element: @_
}

sub characters {
    ### characters: @_
}

}

sub handle_menu {
    my ($self, $e) = @_;

    if (exists $self->{parent_menus}) {
    } else {
        $self->{root_menu} = {};
        $self->{parent_menus} = [];
    }
}

