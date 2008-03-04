#! /usr/bin/perl -w
#
# analyse changes in Debian software repository.
#
# Usage:
#	run `perl apt-update-stat.pl help` for help.
#
# Author:
# 	dieken at newsmth BBS <http://newsmth.net>
#
# Date:
# 	2006-05-21
#
#
#################################################################
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#################################################################
#
#
# Note:
#
# Generally, $pkgs is a hash reference, its elements are also hash
# references named $pkg, $pkg records information about a package.
#
# $pkgs = {
# 	'PkgName' 	=> $pkg = {
# 				'Package' 		=> 'PkgName',
# 				'Priority' 		=> 'required',
# 				...,
# 				'_score' 		=> 300,
#				'_depends'		=> [ [...], ...],
#				'_rdpends'		=> [ .... ],
# 				'_depends_score'	=> 1050,
# 				'_rdepends_score'	=> 2070
# 				},
#	'PkgName2' 	=> $pkg2 = {...},
# 	...
# 	};
#
#
# Layout of Debian Software Package Repository:
#
# TYPE         HOST                    VERSION     COMPONENTS
# --- +-------------------------------+ ------ +---------------------------------+
# deb http://archive.ubuntu.com/ubuntu/ dapper main restricted universe multiverse
# 
# http://archive.ubuntu.com/ubuntu/	(main url)
#                                 |--dists/ (contains package indices of different versions)
#                                 |       |
#                                 |       |--dapper/
#                                 |       |        |--Release
#                                 |       |        |--Release.gpg
#                                 |       |        |--main/   (one of components)
#                                 |       |        |      |--binary-i386/Packages[.gz|.bz2]
#                                 |       |        |      |--...
#                                 |       |        |--restricted/
#                                 |       |                     |--...
#                                 |       |--breezy/
#                                 |
#                                 |--pool/	(real packages and sources are stored here)
#
# The `Release` file records what components and architectures a release version supports,
# it also points out where to find binary package indices and source indices(this program
# doesn't collect information about source packages).
#
# The `Packages` file records information of each package like name, priority, section and
# where to find it.
#
# There is MD5 and SHA1 digest information in `Release` and `Packages` files, but this
# little program doesn't make use of them.
#
# The `Release` files and `Packages` files are downloaded to /var/lib/apt/lists
# by `apt-get update` or `aptitude update`.
#
#
# Summary of each function and their dependence:
#	see comment of each subroutine.
#
# XXX:
# 	don't know how to deal with indirect circular dependence;
# 	%option is not used;
#	no MD5 or SHA1 check;
#	don't take account of "Conflicts", "Recommends", "Suggests" and so on;
#	some Perl tricks for performance ? (I think it's enough already)
#	graphical report? (It seems unnecessary)
#

use strict;
use Carp qw(carp cluck confess);
use Cwd 'abs_path';
use FileHandle;
use File::Glob ':glob';
use File::Path;
use File::Spec;
use LWP::Simple;
use Data::Dumper;


#-----------------------------  MAIN ENTRY -----------------------------------
my %commands = (
	"update"	=> \&do_update,
	"stat"		=> \&do_stat,
	"compare"	=> \&do_compare
);


if (@ARGV < 1 || ! exists $commands{$ARGV[0]}) {
	usage();
	exit 0;
}

my $cmd = shift;

$commands{$cmd}->(@ARGV);


# -------------------------- COMMANDS  ---------------------------------------
sub usage {
	print<<EOF
apt-update-stat.pl  <host>  <version>  <architecture>  <saveToDir>
eg.
	apt-update-stat.pl update http://archive.ubuntu.com/ubuntu dapper i386 ./20060521

apt-update-stat.pl stat  <Packages-file>...
eg.
	apt-update-stat.pl stat ./20060521/*Packages.gz  >stat.log 2>stat.err

apt-update-stat.pl compare  <oldPackagesDir>  <newPackagesDir>
eg.
	apt-update-stat.pl compare ./20060521 ./20060525 >cmp.log 2>cmp.err
	sort -r -n -k 4 cmp.log | less		# order by rdepends score
EOF
}


sub do_update {
	my ($host, $version, $arch, $dir) = @_;
	if (! defined $dir) {
		print "please run `apt-update-stat.pl help` for help.\n";
		return;
	}

	my $release = fetch_Release($host, $version, $dir);
	print "Successfully downloaded ", build_url($host, $version, "Release"), " to $release\n";

	my %spec = parse_Release($release);

	if (! grep /$arch/o, @{$spec{'Architectures'}}) {
		print "wrong arch name, available arch: @{$spec{'Architectures'}}\n";
		return;
	}

	my ($url, $packages_url, $path);
	foreach (@{$spec{'Components'}}) {
		$packages_url = build_Packages_url($_, $arch, ".gz");
		$url=  build_url($host, $version, $packages_url);
		print "fetching $url...\n"; 
		$path = fetch_file($host, $version, $packages_url, $dir);
		print "Successfully fetched to $path\n";
	}
}


sub do_stat {
	if (@_ < 1) {
		print "please run `apt-update-stat.pl help` for help.\n";
		return;
	}

	my %pkgs = ();
	my $Packages_file;
	my $count = 0;
	while ($Packages_file = shift) {
		$count += parse_Packages(\%pkgs, $Packages_file);
	}

	print "$count packages parsed.\n";

	generate_depends_rdepends(\%pkgs);

	# print STDERR Dumper(\%pkgs);

	while (my ($name, $pkg) = each %pkgs) {
		printf "%-40s%5s%15s%15s\n", $name,
		calculate_score(\%pkgs, $name),
		calculate_depends_score(\%pkgs, $name),
		calculate_rdepends_score(\%pkgs, $name);
	}
}


sub do_compare {
	if (@_ < 2) {
		print "please run `apt-update-stat.pl help` for help.\n";
		return;
	}

	my ($olddir, $newdir) = @_;
	my @oldPackages = <$olddir/*Packages.gz>;
	my @newPackages = <$newdir/*Packages.gz>;
	if (@oldPackages < 1 || @newPackages < 1) {
		print "you need some ...Packages.gz files in $olddir and $newdir\n";
		return;
	}

	my %oldpkgs = ();
	my %newpkgs = ();
	my ($oldcount, $newcount) = (0, 0);

	foreach (@oldPackages) {
		$oldcount += parse_Packages(\%oldpkgs, $_);
	}
	foreach (@newPackages) {
		$newcount += parse_Packages(\%newpkgs, $_);
	}

	print "$olddir: \t$oldcount packages\n$newdir: \t$newcount packages.\n";

	# print STDERR Dumper(\%oldpkgs, \%newpkgs);
	
	generate_depends_rdepends(\%oldpkgs);
	generate_depends_rdepends(\%newpkgs);

	while (my ($name, $pkg) = each %newpkgs) {
		if (! exists $oldpkgs{$name}) {
			printf "%-30s %5s %10s %10s %20s \n",
			"${name}[NEW]",
			calculate_score(\%newpkgs, $name),
			calculate_depends_score(\%newpkgs, $name),
			calculate_rdepends_score(\%newpkgs, $name),
			$pkg->{'Version'};
		} elsif ($oldpkgs{$name}->{'Version'} ne $pkg->{'Version'}) {
			printf "%-30s %5s %10s %10s %10s=>%10s\n",
			$name,
			calculate_score(\%newpkgs, $name),
			calculate_depends_score(\%newpkgs, $name),
			calculate_rdepends_score(\%newpkgs, $name),
			$oldpkgs{$name}->{'Version'},
			$pkg->{'Version'};
		}
	}
}



# ----------------------------------SUBROUTINES----------------------------------

# build_listname($host, $version, $file)
# 	build a file name as apt does, see /var/lib/apt/lists.
#
# eg: $name = build_listname("http://archive.ubuntu.com/ubuntu",
# 			"dapper", "Release");
#
# dependence: none
#
sub build_listname {
	my ($host, $version, $file) = @_;
	$host =~ s|^.*?//||;		# trim protocol part, eg "http://"
	$host =~ s|/$||;
	$host =~ tr|/|_|;
	$file =~ tr|/|_|;

	# archive.ubuntu.com_ubuntu_dists_dapper_Release
	return $host . "_dists_" . $version . "_" . $file;
}


# build_Packages_url($component, $architecture, $suffix)
# 	build relative url of a Packages file.
#
# eg. $partial_url = build_Packages_url("main", "i386", ".gz");
#
# dependence: none
#
sub build_Packages_url {
	my ($component, $architecture, $suffix) = @_;

	return $component . "/binary-" . $architecture . "/Packages" . $suffix;
}

# build_url($host, $version, $file)
# 	build a url like http://archive.ubuntu.com/ubuntu/dists/dapper/Release
#
# eg. $full_url = build_url("http://archive.ubuntu.com/ubuntu",
# 				"dapper", "Release");
# dependence: none
#
sub build_url {
	my ($host, $version, $file) = @_;
	$host =~ s|/$||;

	return $host . "/dists/" . $version . "/" . $file;
}


# fetch_file($host, $version, $file, $dir)
# 	fetch "$host/dists/$version/$file", save it to $dir,
# 	and return its full path name.
#
# eg: $full_path = fetch_file("http://archive.ubuntu.com/ubuntu", "dapper",
# 				"Release", "20060521/");
#
# dependence: build_listname(), build_url()
#
sub fetch_file {
	my ($host, $version, $file, $dir) = @_;
	my $path = abs_path($dir);
	my $filename = build_listname($host, $version, $file);
	my $url = build_url($host, $version, $file);

	if (! -e $path) {
		mkpath $path || confess "Can't mkdir $path: $!\n";
	}

	$path = File::Spec->catfile($path, $filename);
	if (is_success(mirror($url, $path))) {
		return $path;
	} else {
		confess "Can't download $url: $!\n";
	}
}


# fetch_Release($host, $version, $dir)
# 	fetch Release file of $version from $host and save to $dir.
# 	return full path name of Release file.
#
# eg. $full_path = fetch_Release("http://archive.ubuntu.com/ubuntu",
# 				"dapper","20060521/");
#
# dependence: fetch_file()
#
sub fetch_Release {
	my ($host, $version, $dir) = @_;

	return fetch_file($host, $version, "Release", $dir);
}


# parse_Release($release_file)
# 	parse Release file, return a hash representation:
# 		(
# 			"Architectures" => [....],
# 			"Components" => [....]
# 		)
#
# eg. %spec = parse_Release("20060521/archive.ubuntu.com_ubuntu_dists_dapper_Release");
#
# dependence: none
#
sub parse_Release {
	my $release = shift;
	my $fh = new FileHandle;
	my %spec= ();
	my $gotit = 0;

	$fh->open("<$release") || confess "Can't open Release file $release: $!\n";
	while (<$fh>) {
		if (/^Architectures: *(.*?) *$/) {
			$spec{"Architectures"} = \@{[split / +/, $1]};
			last if (++$gotit > 1);
		} elsif (/^Components: *(.*?) *$/) {
			$spec{"Components"} = \@{[split / +/, $1]};
			last if (++$gotit > 1);
		}
	}

	undef $fh;

	confess "Wrong Release file: $release\n" if $gotit < 2;

	return %spec;
}


# fetch_Packages($host, $version, $component, $architecture, $dir)
#	fetch Packages file of $version from $host and save to $dir.
#	return full path name of this Packages file.
#
# eg. $full_path = fetch_Packages("http://archive.ubuntu.com/ubuntu", "dapper",
# 				"main", "i386", "20060521/");
#
# dependence: fetch_file(), build_Package_url()
#
# XXX: let caller decide which format to download: Packages, Packages.gz
# or Packages.bz2
#
sub fetch_Packages {
	my ($host, $version, $comp, $arch, $dir) = @_;

	return fetch_file($host, $version,
			build_Packages_url($comp, $arch, ".gz"),
			$dir);
}


# parse_Packages(\%packages, $filename, %option);
#	parse Packages file $filename, add information to %packages
#	according to %option.
#
# eg. $count_addition = parse_Packages(\%pkgs,
# 		"20060521/archive.ubuntu.com_ubuntu_dists_dapper_main_binary-i386_Packages.gz");
#
# dependence: none
#
sub parse_Packages {
	my ($pkgs, $filename, %option) = @_;
	my $fh;
	my $count;
	my $pkg;
	my $keyword;

	if ($filename =~ /Packages\.gz$/) {
		$fh = new FileHandle "gzip -dc $filename |";
	} elsif ($filename =~ /Packages\.bz2$/) {
		$fh = new FileHandle "bzip2 -dc $filename |";
	} elsif ($filename =~ /Packages$/) {
		$fh = new FileHandle "< $filename";
	} else {
		confess "Unknown Packages file format: $filename\n";
	}

	if (! defined($fh)) {
		carp "Can't read $filename: $!\n";
		return 0;
	}

	$count = 0;
	$pkg = {};

	while (<$fh>) {
		if (/^\s*$/) {
			# avoid multiple continuous white line.
			if (exists $pkg->{'Package'}) {
				$pkgs->{$pkg->{'Package'}} = $pkg;
				$pkg = {};
				++$count;
			}
			next;
		} elsif (/^ /) {	# skip description
			next;
		}

		foreach $keyword ('Package', 'Priority', 'Section', 'Essential',
							'Version', 'Depends', 'Conflicts',
							'Pre-Depends', 'Recommends', 'Suggests') {
			if (/^$keyword:\s*(.*)\s*$/) {
				$pkg->{$keyword} = $1;
				last;
			}
		}
	}

	undef $fh;

	return $count;
}


# test_parse_Packages($filename)
# 	test case for parse_Packages() subroutine.
#
sub test_parse_Packages {
	my %pkgs = ();
	
	confess "Usage: test_parse_Packages \$Packages_filename\n" if @_ < 1;

	my $count = parse_Packages(\%pkgs, shift);
	print "$count packages\n";

	while (my ($name, $pkg) = each %pkgs) {
		print "=======================$name==============================\n";
		while (my ($key, $value) = each %$pkg) {
			print "$key => $value\n"
		}
	}
}

# test_parse_Packages(shift);
# exit 0;


# calculate_score(\%pkgs, $name, %SCORE)
# 	calculate importance of a package according to its section,
# 	priority and so on.
#
# eg. $score = calculate_score(\%pkgs, $pkg_name);
#
# dependence: none
#
# note: must call parse_Packages() first  to get a %pkgs.
#
sub calculate_score {
	my ($pkgs, $name, %SCORE) = @_;
	my $pkg = $pkgs->{$name};
	
	if (! defined($pkg)) {
		cluck "WARN: Package \`$name\` is not found, take zero as its score.\n";
		return 0;
	}

	return $pkg->{'_score'} if exists $pkg->{'_score'};

	$SCORE{'Essential'} = 20  if ! exists $SCORE{'Essential'};
	# Priority:
	$SCORE{'required'} 	= 100 if ! exists $SCORE{'required'};
	$SCORE{'standard'} 	= 80  if ! exists $SCORE{'standard'};
   	$SCORE{'important'} = 60  if ! exists $SCORE{'important'};
   	$SCORE{'extra'} 	= 40  if ! exists $SCORE{'extra'};
   	$SCORE{'optional'} 	= 20  if ! exists $SCORE{'optional'};
	# Section
	$SCORE{'base'} 	= 5 if ! exists $SCORE{'base'};
   	$SCORE{'admin'}	= 2 if ! exists $SCORE{'admin'};
   	$SCORE{'libs'} 	= 2 if ! exists $SCORE{'libs'};
	$SCORE{'gnome'}	= 1 if ! exists $SCORE{'gnome'};
	$SCORE{'kde'}	= 1 if ! exists $SCORE{'kde'};
	$SCORE{'x11'}	= 1 if ! exists $SCORE{'x11'};

	$pkg->{'_score'} = 0;

	$pkg->{'_score'} += $SCORE{'Essential'} if exists $pkg->{'Essential'};
	$pkg->{'_score'} += $SCORE{$pkg->{'Section'}} if exists $SCORE{$pkg->{'Section'}};
	$pkg->{'_score'} += $SCORE{$pkg->{'Priority'}} if exists $SCORE{$pkg->{'Priority'}};

	return $pkg->{'_score'};
}


# generate_depends_rdepends(\%pkgs)
# 	generate depends and rdepends list for each package.
# 	$pkg = {
# 		"Package" => "PkgName",
# 		...
# 		"_depends" => [ [pkg_name, version_condition],
# 				[pkg2_name, version_condition,
# 				 alternative_pkg2_name, version_condition]
# 			      ],
# 		"_rdepends" => [pkgA_name, pkgB_name],
# 	};
#
# eg. generate_depends_rdepends(\%pkgs);
#
# dependence: none
#
# note: must call parse_Packages() first to get a %pkgs.
#
# XXX: There are some APIs to parse /var/cache/apt/pkgcache.bin, see
# package `libapt-pkg-dev` and package `libapt-pkg-perl`.
#
sub generate_depends_rdepends {
	my ($pkgs) = shift;
	my ($name, $pkg, @Depends, @deps);
	my ($p_name, $p_ver, $p);

	while (($name, $pkg) = each %$pkgs) {
		next if ! exists $pkg->{'Depends'};

		$pkg->{'_depends'} = [];
		# eg. @Depends = ("a (>> 1.1) | b (>> 2.1)", "c (>> 2.1)", "d")
		@Depends = split /,\s*/, $pkg->{'Depends'};
		foreach (@Depends) {
			# eg. @deps = ("a (>> 1.1)", "b (>> 2.1)")
			# eg. $p = ["a", ">> 1.1",
			# 	    "b", ">> 2.1"];
			$p = [];
			@deps = split /\s*\|\s*/;
			foreach (@deps) {
				# eg. ("a", ">> 1.1")
				# $p_ver may be undef

				($p_name, $p_ver) = /([^\s]+)\s*(?:\((.*)\))?/;
				push @$p, $p_name, $p_ver;

				if (!defined($p_name)) {
					print $_, "\n";
					print "p_name is undef: $pkg->{'Depends'}\n";
					next;
				}
				next if ! exists $pkgs->{$p_name};

				if (exists $pkgs->{$p_name}->{'_rdepends'}) {
					push @{$pkgs->{$p_name}->{'_rdepends'}}, $name;
				} else {
					$pkgs->{$p_name}->{'_rdepends'} = [$name];
				}
			}
			push @{$pkg->{'_depends'}}, $p;
		}
	}
}


# test_generate_depends_rdepends($oldPackages, $newPackages)
# 	test case for generate_depends_rdepends()
#
sub test_generate_depends_rdepends {
	my %pkgs = ();
	my $count;

	confess "Usage: test_generate_depends_rdepends \$Packages_filename\n"
		if @_ < 1;

	$count = parse_Packages(\%pkgs, shift);

	print "count=$count\n";

	generate_depends_rdepends(\%pkgs);

	# $Data::Dumper::Maxdepth = 4;
	print Dumper(\%pkgs);
}

# test_generate_depends_rdepends(@ARGV);
# exit 0;


# calculate_depends_score(\%pkgs, $name)
# 	calculate how much a package depends on other packages.
#
# eg. $depends_score = calculate_depends_score(\%pkgs, $pkg_name);
#
# dependence: calculate_score()
#
# note: must call generate_depends_rdepends() first.
#
sub calculate_depends_score {
	my ($pkgs, $name) = @_;
	my $pkg = $pkgs->{$name};

	if (! defined($pkg)) {
		cluck "WARN: Package \`$name\` is not found, take zero as its depends score.\n";
		return 0;
	}

	return $pkg->{'_depends_score'} if exists $pkg->{'_depends_score'};

	$pkg->{'_depends_score'} = 0;
	return $pkg->{'_depends_score'} if ! exists $pkg->{'Depends'};

	my ($alternative_depends, $maxscore, $score, $i, $p_name);

	foreach $alternative_depends (@{$pkg->{'_depends'}}) {
		$maxscore = 0;
		#eg. $alternative_depends = [libc5, "1.2", "libc6", "2.3.2"];
		for ($i = 0; $i < @$alternative_depends; $i += 2) {
			$p_name = $alternative_depends->[$i];
			$score = calculate_depends_score($pkgs, $p_name);
			if ($score >= 0) {
				$score += calculate_score($pkgs, $p_name);
				$maxscore = $score if $maxscore < $score;
			} else {
				my $found = 0;
				foreach (@{$pkg->{'_rdepends'}}) {
					if ($p_name eq $_) {
						$found = 1;
						last;
					}
				}

				if ($found) {
					# direct dependence is allowed, these
					# two packages can be regarded as one
					# package.
				} else {
					warn "ERROR! Circular dependence found: [$name] on [$p_name]\n";
					# so we will get the wrong dependence path.
					$pkg->{'_depends_score'} = - 100000000;
					return $pkg->{'_depends_score'};
				}
			}
		} # end for
		$pkg->{'_depends_score'} -= $maxscore;
	}

	# ok, there is no circular dependence.
	$pkg->{'_depends_score'} = - $pkg->{'_depends_score'};

	return $pkg->{'_depends_score'};
}


# calculate_rdepends_score(\%pkgs, $name)
# 	calculate how much a package is depended on by other packages.
#
# eg. $rdepends_score = calculate_rdepends_score(\%pkgs, $pkg_name);
#
# dependence: calculate_score()
#
# note: must call generate_depends_rdepends() first.
#
sub calculate_rdepends_score {
	my ($pkgs, $name) = @_;
	my $pkg = $pkgs->{$name};

	if (! defined($pkg)) {
		cluck "WARN: Package \`$name\` is not found, take zero as its rdepends score.\n";
		return 0;
	}

	return $pkg->{'_rdepends_score'} if exists $pkg->{'_rdepends_score'};

	$pkg->{'_rdepends_score'} = 0;
	return $pkg->{'_rdepends_score'} if ! exists $pkg->{'_rdepends'};

	my ($p_name, $score);

	foreach $p_name (@{$pkg->{_rdepends}}) {
		$score = calculate_rdepends_score($pkgs, $p_name);
		if ($score >= 0) {
			$pkg->{'_rdepends_score'} -= $score + calculate_score($pkgs, $p_name);
		} else {
			my $found = 0;
			foreach (@{$pkgs->{$p_name}->{'_rdepends'}}) {
				if ($name eq $_) {
					$found = 1;
					last;
				}
			}

			if ($found) {
				# direct dependence is allowed, these
				# two packages can be regarded as one
				# package.
			} else {
				warn "ERROR! Circular dependence found: [$name] by [$p_name]\n";
				# so we will get the wrong dependence path.
				$pkg->{'_rdepends_score'} = - 100000000;
				return $pkg->{'_rdepends_score'};
			}
		}
	}

	# ok, there is no circular dependence.
	$pkg->{'_rdepends_score'} = - $pkg-> {'_rdepends_score'};

	return $pkg->{'_rdepends_score'};
}


# test_calculate_depends_rdpends_score($Packages_file)
# 	test whether calculate_depends_score() and calculate_rdepends_score()
# are implemented properly.
#
sub test_calculate_depends_rdpends_score {
	my (%pkgs, $count);

	confess "Usage: test_calculate_depends_rdpends_score \$Packages_filename\n"
		if @_ < 1;

	$count = parse_Packages(\%pkgs, shift);

	# print "count=$count\n";

	generate_depends_rdepends(\%pkgs);

	while (my ($name, $pkg) = each %pkgs) {
		print "$name\t\t", calculate_score(\%pkgs, $name),
			"\t", calculate_depends_score(\%pkgs, $name),
			"\t", calculate_rdepends_score(\%pkgs, $name),
			"\n";
	}
}

# test_calculate_depends_rdpends_score(@ARGV);
# exit(0);


# compare_Packages(\%oldPackages, \%newPackages, %option)
# 	compare Packages of different version, for example, yesterday's and
# 	today's.
#	return difference of two Packeges hash.
#
sub compare_Packages {
	my ($oldpkgs, $newpkgs, %option) = @_;
	my ($oldver, $newver);

	foreach my $name (keys %$newpkgs) {
		next if (! exists($oldpkgs->{$name}));
		$oldver = $oldpkgs->{$name}->{"Version"};
		$newver = $newpkgs->{$name}->{"Version"};
		if ($oldver ne $newver) {
			print "$name: $oldver => $newver\n";
		}
	}
}


# test_compare_Packages($oldPackages, $newPackages)
# 	test case for compare_Packages().
#
sub test_compare_Packages {
	my (%oldpkgs, %newpkgs) = ();
	my ($oldcount, $newcount);

	confess "Usage: test_compare_Packages ",
			"\$old_Packages_filename \$new_Packages_filename\n"
		if @_ < 2;

	$oldcount = parse_Packages(\%oldpkgs, shift);
	$newcount = parse_Packages(\%newpkgs, shift);

	print "old count=$oldcount, new count=$newcount\n";

	compare_Packages(\%oldpkgs, \%newpkgs);
}

# test_compare_Packages(@ARGV);
# exit 0;



# vi:set ts=8 sw=8 noet nowrap ft=perl:

