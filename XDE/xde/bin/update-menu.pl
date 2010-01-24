#!/usr/bin/perl
{ # begin package
use Getopt::Long;
use strict;
use warnings;

BEGIN {
    MenuBuilder->import();
}


my $root_menu = MenuBuilder::build_from_uri($ARGV[0]);


exit 0;

} # end package

################ MenuBuilder ###################################
{ # begin package
package MenuBuilder;
use File::Find;
#use Smart::Comments;
use XML::SAX;
use strict;
use warnings;

BEGIN {
    MenuXMLHandler->import();
}

sub build_from_uri {
    my $uri = shift;

    die "No uri specified!\n" if ! defined $uri;

    my $handler = new MenuXMLHandler();

    XML::SAX::ParserFactory->parser(Handler => $handler)->parse_uri($ARGV[0]);

    #my $app = { Filename => 'feh.desktop', Categories => ['Graphics', 'Viewer']};
    #place_application($handler->root_menu, $app);

    place_applications($handler->root_menu, [], []);

    return $handler->root_menu;
}

#--------------------------------------------------------------
# $menu     a Menu object
# $apps     [
#               {   # applications found in this menu's application dirs
#                   Filename    => {    # a parsed .desktop file
#                                   Name => ...,
#                                  },
#                   ...
#               },
#               ....
#           }
# $dirs     [
#               {   # directories found in this menu's directory dirs
#                   Filename    => {    # a parsed .directory file
#                                   ....
#                                   },
#                   ...
#               },
#               ...
#           ]
sub place_applications {
    my ($menu, $apps, $dirs) = @_;

    push @$apps, find_entries('\.desktop', @{$menu->appdirs});
    push @$dirs, find_entries('\.directory', @{$menu->dirdirs});

    if (defined $menu->directory) {
        for (my $i = @$dirs - 1; $i >= 0; --$i) {
            my $entries = $dirs->[$i];

            while (my ($Filename, $entry) = each %$entries) {
                if ($menu->directory eq $Filename) {
                    print "  " x (@$dirs - 1), $menu->name, " ", $entry->{Fullpath}, "\n";
                    $menu->menu_dir($entry);
                    keys %$entries;     # reset hash iterator
                    last;
                }
            }

            last if defined $menu->menu_dir;
        }
    }

    if (defined $menu->matchsub) {
        for my $entries (@$apps) {
            while (my ($Filename, $entry) = each %$entries) {
                if ($menu->matchsub->($entry)) {
                    print "  " x @$apps, $menu->name, " ", $entry->{Fullpath}, "\n";
                    delete $entries->{$Filename};
                    $menu->menu_items($Filename, $entry);
                }
            }
        }
    }

    for my $child (@{$menu->children}) {
        place_applications($child, $apps, $dirs);
    }

    pop @$dirs;
    my $entries = pop @$apps;

    while (my ($Filename, $entry) = each %$entries) {
        print STDERR "WARN: application: ", $entry->{Fullpath}, " not placed!\n";
    }
}

sub place_application {
    my ($menu, $app) = @_;

    if (defined $menu->matchsub) {
        if ($menu->matchsub->($app)) {
            print STDERR "matched: ", $menu->name, "\n";
            return 1;
        }
    }

    for my $child (@{$menu->children}) {
        return 1 if (match($app, $child))
    }

    return 0;
}

sub find_entries {
    my ($suffix, @dirs) = @_;
    my %entries = ();

    for my $d (@dirs) {
        $d =~ s/[\\\/]+$//g;
        next if ! -d $d;

        find(sub {
                return if -d $_;
                return if ! /$suffix$/;

                open my $fh, $_ or return;
                my %entry = ();
                while (my $line = <$fh>) {
                    chomp $line;
                    my ($k, $v) = split /=/, $line, 2;
                    next if !defined $v;
                    $entry{$k} = $v;
                }

                if (exists $entry{Categories}) {
                    my $categories = $entry{Categories};
                    $categories =~ s/^;+|;+$//g;
                    $entry{Categories} = [split /;/, $categories];
                }

                $entry{Fullpath} = $File::Find::name;
                $entry{Filename} = $File::Find::name;
                $entry{Filename} =~ s/^\Q$d\E\///;
                $entries{$entry{Filename}} = \%entry;
            }, $d);
    }

    return \%entries;
}

} # end package

############## MenuXMLHandler #################################
# http://standards.freedesktop.org/menu-spec/latest
{ # begin package
package MenuXMLHandler;
use base qw/XML::SAX::Base/;
use Class::Struct Menu => [parent   => 'Menu',
                           children => '*@',
                           name     => '$',
                           directory=> '$',
                           appdirs  => '*@',
                           dirdirs  => '*@',
                           mergedirs=> '*@',
                           matchsub => '$',         # subroutine reference, accept a hash ref to a .desktop info
                           menu_items   => '*%',    # hash ref of .desktop infos
                           menu_dir     => '$',     # hash ref to a .directory info
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


sub root_menu {
    return $_[0]{root_menu};
}


=pod
After XML::SAX parsed a XML file, this handler contains:
    { root_menu   => Root_Menu_Object }
use root_menu() method to obtain the root menu object.

=cut
} # end package

