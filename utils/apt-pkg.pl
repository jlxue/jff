#!/usr/bin/perl
#
#  See show_usage().
#
use AptPkg::Cache;
use Getopt::Long;
use Smart::Comments;
use strict;
use warnings;

(my $self = $0) =~ s#.*/##;

my $cache = new AptPkg::Cache();

if (@ARGV == 0) {
    show_usage();
} elsif ($ARGV[0] eq 'stat') {
    stat_installed_size($cache);
} elsif ($ARGV[0] eq 'depends') {
    shift;
    check_depends($cache);
} else {
    show_usage();
}

1;


#----------------------------------------------------------------
sub show_usage {
    print STDERR <<EOF;
Usage:
    $self stat
        check space usage of all installed packages.

    $self depends FILE...
        check space usage by specified packages and their dependency.

        FILE contains a list of package names, one per line,
        if no FILE argument given, standard input is used.

        If the first FILE doesn't exist, then the file arguments
        are thought as package names.
EOF
}


sub stat_installed_size {
    my $cache = shift;

    my $total_installed_size = 0;
    my $core_installed_size = 0;
    my $libs_installed_size = 0;
    while (my ($name, $pkg) = each %$cache) {
        if (AptPkg::State::Installed == $pkg->{CurrentState}) {
            accum_installed_size(\$total_installed_size,
                                 \$core_installed_size,
                                 \$libs_installed_size,
                                 $pkg->{CurrentVer});
        }
    }

    output_installed_size($total_installed_size, $core_installed_size, $libs_installed_size);
}


sub check_depends {
    my $cache = shift;
    my $policy = $cache->policy();

    my @pkg_names = ();
    my %flags = ();
    my @candidates = ();

    my $total_installed_size = 0;
    my $core_installed_size = 0;
    my $libs_installed_size = 0;
    my $installed_installed_size = 0;

    my $opt_recommends = 1;
    my $opt_suggests = 0;
    my $opt_core = 0;

    GetOptions("recommends!"   => \$opt_recommends,
               "suggests!"     => \$opt_suggests,
               "core!"         => \$opt_core);

    # collect pkg names specified by user
    if (@ARGV > 0 && ! -r $ARGV[0]) {
        @pkg_names = @ARGV;
    } else {
        while (<>) {
            chomp;
            push @pkg_names, $_;
        }
    }

    # add all packages specified by user to @candidates
    for my $name (@pkg_names) {
        if (! exists $cache->{$name}) {
            print STDERR "Warning: \"$name\" not found!\n";
            next;
        }

        next if exists $flags{$name};
        $flags{$name} = 0;

        my $pkg = $cache->{$name};
        my $candidate = $policy->candidate($pkg);
        push @candidates, $candidate if defined $candidate;
    }

    # add dependency to @candidates
    while (my $candidate = shift @candidates) {
        # accumulate $total_installed_size, $core_installed_size and $libs_installed_size
        accum_installed_size(\$total_installed_size,
                             \$core_installed_size,
                             \$libs_installed_size,
                             $candidate);
        if (AptPkg::State::Installed == $candidate->{ParentPkg}{CurrentState}) {
            $installed_installed_size += $candidate->{InstalledSize};
        }

        # process dependency
        my $depends = $candidate->{DependsList};
        my $is_alternative = 0;
        for my $depend (@$depends) {
            my $depType = $depend->{DepType};

            # ignore not requried dependency
            if ($depType == AptPkg::Dep::Depends) {
            } elsif ($depType == AptPkg::Dep::PreDepends) {
            } elsif ($depType == AptPkg::Dep::Recommends) {
                next if !$opt_recommends;
            } elsif ($depType == AptPkg::Dep::Suggests) {
                next if !$opt_suggests;
            } else {
                # XXX: don't care about Conflicts and Replaces
                next;
            }

            # process alternative dependency: pkgA | pkgB | pkgC
            if ($is_alternative) {
                $is_alternative = 0 if AptPkg::Dep::Or != ($depend->{CompType} & AptPkg::Dep::Or);
                next;   # XXX: always install first alternative
            } else {
                $is_alternative = 1 if AptPkg::Dep::Or == ($depend->{CompType} & AptPkg::Dep::Or);
            }

            # ignore dependency that has already been processed
            my $pkg = $depend->{TargetPkg};
            if (exists $flags{$pkg->{Name}}) {
                $flags{$pkg->{Name}}++;
                next;
            } else {
                $flags{$pkg->{Name}} = 0;
            }

            # add new dependency to @candidates
            my $candidate = $policy->candidate($pkg);
            push @candidates, $candidate if defined $candidate;
        }
    }

    $installed_installed_size /= 1024 * 1024;
    print <<EOF;
Already installed packages' installed size: $installed_installed_size MB
EOF
    output_installed_size($total_installed_size, $core_installed_size, $libs_installed_size);
    output_package_list($cache, %flags);
}


sub accum_installed_size {
    my ($total_installed_size, $core_installed_size, $libs_installed_size, $ver) = @_;

    my $installed_size = $ver->{InstalledSize};
    my $priority = $ver->{Priority};
    my $section = $ver->{Section};

    $$total_installed_size += $installed_size;

    if ($priority == AptPkg::State::Important || $priority == AptPkg::State::Required ||
            $priority == AptPkg::State::Standard) {
        $$core_installed_size += $installed_size;
    }

    if ($section eq 'libs') {
        $$libs_installed_size += $installed_size;
    }
}


sub output_installed_size {
    my ($total_installed_size, $core_installed_size, $libs_installed_size) = @_;

    for ($total_installed_size, $core_installed_size, $libs_installed_size) {
        $_ /= 1024 * 1024;
    }

    print <<EOF;
                      Total installed size: $total_installed_size MB
 Important, required and standard priority: $core_installed_size MB
                            Section "libs": $libs_installed_size MB

EOF
}


sub output_package_list {
    my ($cache, %flags) = @_;
    my $policy = $cache->policy();

    my @names = sort keys %flags;

    my ($flag, $name, $version, $section, $priority, $installed_size, $count);

    printf "    %-35s %-25s %-20s %-9s %-14s %-s\n",
            "Name", "Version", "Section", "Priority", "Installed-Size", "Direct-Depends";
    print '=' x 120, "\n";
    for (@names) {
        my $pkg = $cache->{$_};
        my $candidate = $policy->candidate($pkg);
        next if !defined $candidate;

        $flag = generate_state_flag($pkg->{CurrentState},
                                    $pkg->{SelectedState},
                                    $pkg->{Flags});
        $name = $pkg->{Name};
        $version = $candidate->{VerStr};
        $section = $pkg->{Section};
        $priority = $candidate->{Priority};
        $installed_size = $candidate->{InstalledSize};
        $count = $flags{$name};

        printf "%-s %-35s %-25s %-20s %-9s %14d %d\n",
                $flag, $name, $version, $section, $priority, $installed_size, $count;
    }
}


sub generate_state_flag {
    my ($CurrentState, $SelectedState, $Flags) = @_;
    my $s = "";

    if ($CurrentState == AptPkg::State::NotInstalled) {
        $s .= 'p';
    } elsif ($CurrentState == AptPkg::State::Installed) {
        $s .= 'i';
    } elsif ($CurrentState == AptPkg::State::ConfigFiles) {
        $s .= 'c';
    } elsif ($CurrentState == AptPkg::State::UnPacked) {
        $s .= 'u';
    } elsif ($CurrentState == AptPkg::State::HalfConfigured) {
        $s .= 'C';
    } elsif ($CurrentState == AptPkg::State::HalfInstalled) {
        $s .= 'H';
    } else {
        $s .= '-';
    }

    if ($SelectedState == AptPkg::State::Install) {
        $s .= 'i';
    } elsif ($SelectedState == AptPkg::State::Hold) {
        $s .= 'h';
    } elsif ($SelectedState == AptPkg::State::DeInstall) {
        $s .= 'd';
    } elsif ($SelectedState == AptPkg::State::Purge) {
        $s .= 'p';
    } else {
        $s .= '-';
    }

    if (($Flags & AptPkg::Flag::Auto) == AptPkg::Flag::Auto) {
        $s .= '-';
    } else {
        $s .= 'A';
    }

    return $s;
}

