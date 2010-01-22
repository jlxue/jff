#!/usr/bin/perl
{
use XML::SAX;
use strict;
use warnings;

BEGIN {
    MenuXMLHandler->import();
};

die "Usage: $0 menu-file\n" if @ARGV == 0;

my $handler = new MenuXMLHandler();

XML::SAX::ParserFactory->parser(Handler => $handler)->parse_uri($ARGV[0]);

my $app = { Filename => 'feh.desktop', Categories => ['Graphics', 'Viewer']};

match($app, $handler->{root_menu});

exit 0;

###############################################################
sub match {
    my ($app, $menu) = @_;

    if (defined $menu->matchsub) {
        if ($menu->matchsub->($app)) {
            print "matched: ", $menu->name, "\n";
            return 1;
        }
    }

    for my $child (@{$menu->children}) {
        return 1 if (match($app, $child))
    }

    return 0;
}

}

###############################################################
# http://standards.freedesktop.org/menu-spec/latest
{ # begin package
=pod
After XML::SAX parsed a XML file, this handler contains:
{ root_menu   => Root_Menu_Object }
=cut
package MenuXMLHandler;
use base qw/XML::SAX::Base/;
use Class::Struct Menu => [parent   => 'Menu',
                           children => '*@',
                           name     => '$',
                           directory=> '$',
                           appdirs  => '*@',
                           dirdirs  => '*@',
                           mergedirs=> '*@',
                           matchsub => '$',     # subroutine reference
                          ];
use File::Spec;
use List::Util;
#use Smart::Comments;
use strict;
use warnings;


sub start_document {
    my $self = shift;
    $self->{root_menu} = new Menu;
    $self->{current_menu} = $self->{root_menu};
}

sub end_document {
    my $self = $_[0];
    delete $self->{characters};
    delete $self->{current_menu};
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

    $self->{stack} = [];
}

sub handle_end_Menu {
    my $stack = $_[0]{stack};

    if (defined $stack) {
        my $exp = join(' && ', @$stack);
        die "Empty patterns for $_[0]{current_menu}->name!\n" if length($exp) == 0;
        $_[0]{current_menu}->matchsub(eval "sub { $exp }");
    }

    delete $_[0]{stack};

    $_[0]{current_menu} = $_[0]{current_menu}->parent;
}

sub handle_end_Name {
    $_[0]{current_menu}->name($_[0]->{characters});
}

sub handle_end_Directory {
    $_[0]{current_menu}->directory($_[0]->{characters});
}

sub handle_start_Include {
    push @{$_[0]{stack}}, '(';
}

sub handle_end_Include {
    process_pattern_varargs($_[0]{stack}, '||');
}

sub handle_start_Exclude {
    push @{$_[0]{stack}}, '(';
}

sub handle_end_Exclude {
    process_pattern_varargs($_[0]{stack}, '||');
    my $stack = $_[0]{stack};
    my $exp = pop @$stack;
    push @$stack, "(! $exp)";
}

sub handle_start_And {
    push @{$_[0]{stack}}, '(';
}

sub handle_end_And {
    process_pattern_varargs($_[0]{stack}, '&&');
}

sub handle_end_Not {
    my $stack = $_[0]{stack};
    my $exp = pop @$stack;
    push @$stack, "(! $exp)";
}

sub handle_start_Or {
    push @{$_[0]{stack}}, '(';
}

sub handle_end_Or {
    process_pattern_varargs($_[0]{stack}, '||');
}

sub handle_end_Category {
    push @{$_[0]{stack}}, "Category(\$_[0], \'$_[0]{characters}\')";
}

sub handle_end_Filename {
    push @{$_[0]{stack}}, "Filename(\$_[0], \'$_[0]{characters}\')";
}

sub handle_end_DefaultAppDirs {
    my @appdirs = map { File::Spec->catdir($_, "applications") } xdg_data_dirs();
    push @{$_[0]{current_menu}->appdirs}, @appdirs;
}

sub handle_end_DefaultDirectoryDirs {
    my @dirdirs = map { File::Spec->catdir($_, "desktop-directories") } xdg_data_dirs();
    push @{$_[0]{current_menu}->dirdirs}, @dirdirs;
}

sub handle_end_DefaultMergeDirs {
    my @mergedirs = map { File::Spec->catdir($_, "menus/applications-merged") } xdg_config_dirs();
    push @{$_[0]{current_menu}->mergedirs}, @mergedirs;
}

# http://standards.freedesktop.org/basedir-spec/latest/
my @xdg_config_dirs;
sub xdg_config_dirs {
    if (! @xdg_config_dirs) {
        if (exists $ENV{XDG_CONFIG_DIRS}) {
            my $dirs = $ENV{XDG_CONFIG_DIRS};
            $dirs =~ s/^[\s:]//g;
            $dirs =~ s/[\s:]$//g;
            @xdg_config_dirs = split /:/, $dirs;
        } else {
            @xdg_config_dirs = "/etc/xdg";
        }

        if (exists $ENV{XDG_CONFIG_HOME}) {
            unshift @xdg_config_dirs, $ENV{XDG_CONFIG_HOME};
        } else {
            unshift @xdg_config_dirs, "$ENV{HOME}/.config";
        }
    }

    return @xdg_config_dirs;
}

my @xdg_data_dirs;
sub xdg_data_dirs {
    if (! @xdg_data_dirs) {
        if (exists $ENV{XDG_DATA_DIRS}) {
            my $dirs = $ENV{XDG_DATA_DIRS};
            $dirs =~ s/^[\s:]//g;
            $dirs =~ s/[\s:]$//g;
            @xdg_data_dirs = split /:/, $dirs;
        } else {
            @xdg_data_dirs = qw(usr/local/share/ /usr/share/);
        }

        if (exists $ENV{XDG_DATA_HOME}) {
            unshift @xdg_data_dirs, $ENV{XDG_DATA_HOME};
        } else {
            unshift @xdg_data_dirs, "$ENV{HOME}/.data/share";
        }
    }

    return @xdg_data_dirs;
}

sub process_pattern_varargs {
    my ($stack, $op) = @_;
    my @args = ();

    while (my $exp = pop @$stack) {
        if ($exp eq '(') {
            push @$stack, '(' . join(" $op ", @args) . ')';
            last;
        } else {
            unshift @args, $exp;
        }
    }
}

sub Category {
    return defined(List::Util::first { $_ eq $_[1] } @{$_[0]{Categories}});
}

sub Filename {
    return $_[0]{Filename} eq $_[1];
}

} # end package

