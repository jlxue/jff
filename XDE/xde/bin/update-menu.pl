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
{ # begin package
package MenuXMLHandler;
use base qw/XML::SAX::Base/;
use Class::Struct Menu => [parent   => 'Menu',
                           children => '*@',
                           name     => '$',
                           directory=> '$',
                          ];
use Data::Dumper;
use Smart::Comments;
use strict;
use warnings;


sub start_document {
    my $self = shift;
    $self->{root_menu} = new Menu;
    $self->{current_menu} = $self->{root_menu};
}

sub end_document {
    ### end_document: @_
}

sub start_element {
    my ($self, $e) = @_;
    my $localname = $e->{LocalName};

    $self->{characters} = '';

    my $sub = $self->can("handle_start_$localname");
    $sub->(@_) if defined $sub;
}

sub end_element {
    my ($self, $e) = @_;
    my $localname = $e->{LocalName};

    my $sub = $self->can("handle_end_$localname");
    $sub->(@_) if defined $sub;
}

sub characters {
    $_[0]{characters} .= $_[1]{Data};
}

sub handle_start_Menu {
    my ($self, $e) = @_;
    my $parent = $self->{current_menu};
    my $menu = new Menu(parent => $parent);

    push @{$parent->children()}, $menu;
    $self->{current_menu} = $menu;
}

sub handle_end_Menu {
    $_[0]{current_menu} = $_[0]{current_menu}->parent;
}

sub handle_end_Name {
    $_[0]{current_menu}->name($_[0]->{characters});
}

sub handle_end_Directory {
    $_[0]{current_menu}->directory($_[0]->{characters});
}

sub handle_start_Include {
    $_[0]{operators} = [];
    $_[0]{operands} = [];
    $_[0]{expression} = "";
}

sub handle_end_Include {
}

sub handle_start_And {
    push @{$_[0]{operators}}, 'Or';
    push @{$_[0]{operands}}, 'Or';
}

sub handle_start_Not {
    push @{$_[0]{operators}}, 'Not';
}

sub handle_end_Category {
    push @{$_[0]{operators}}, 'Category';
    push @{$_[0]{operands}}, $_[0]{characters};
}

sub handle_end_Filename {
    push @{$_[0]{operators}}, 'Filename';
    push @{$_[0]{operands}}, $_[0]{characters};
}

} # end package

