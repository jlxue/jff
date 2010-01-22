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
                           condition=> '*@',
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
    if (defined $sub) {
        $sub->(@_);
    } else {
        warn "<$localname> not processed!\n" if !defined $self->can("handle_end_$localname");
    }
}

sub end_element {
    my ($self, $e) = @_;
    my $localname = $e->{LocalName};

    my $sub = $self->can("handle_end_$localname");
    if (defined $sub) {
        $sub->(@_);
    } else {
        warn "</$localname> not processed!\n" if !defined $self->can("handle_start_$localname");
    }
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
    $_[0]{expression} = ["(Include"];
}

sub handle_end_Include {
    push @{$_[0]{expression}}, ")";
    push @{$_[0]{current_menu}->condition}, $_[0]{expression};
}

sub handle_start_Exclude {
    $_[0]{expression} = ["(Exclude"];
    push @{$_[0]{current_menu}->condition}, $_[0]{expression};
}

sub handle_end_Exclude {
    push @{$_[0]{expression}}, ")";
    push @{$_[0]{current_menu}->condition}, $_[0]{expression};
}

sub handle_start_And {
    push @{$_[0]{expression}}, "(And";
}

sub handle_end_And {
    push @{$_[0]{expression}}, ")";
}

sub handle_start_Not {
    push @{$_[0]{expression}}, "(Not";
}

sub handle_end_Not {
    push @{$_[0]{expression}}, ")";
}

sub handle_start_Or {
    push @{$_[0]{expression}}, "(Or";
}

sub handle_end_Or {
    push @{$_[0]{expression}}, ")";
}

sub handle_start_Category {
    push @{$_[0]{expression}}, "(Category";
}

sub handle_end_Category {
    push @{$_[0]{expression}}, $_[0]{characters}, ")";
}

sub handle_start_Filename {
    push @{$_[0]{expression}}, "(Filename";
}

sub handle_end_Filename {
    push @{$_[0]{expression}}, $_[0]{characters}, ")";
}

} # end package

