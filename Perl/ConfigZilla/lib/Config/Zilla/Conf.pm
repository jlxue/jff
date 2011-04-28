package Config::Zilla::Conf;

use strict;
use warnings;
use Params::Check;

my ($default_work_dir, $default_bin_dir, $default_lib_dir,
    $default_inputs_dir, $default_log_dir, $default_lock_dir,
    $default_backup_dir, $default_cache_dir);


if ($^O =~ /MSWin32/) {
    die "Can't find HOMEDRIVE and HOMEPATH environment variables!\n" if
            !exists $ENV{HOMEDRIVE} or !exists $ENV{HOMEPATH};

    my $home = $ENV{HOMEDRIVE} . $ENV{HOMEPATH};
    $g_work_dir = File::Spec->catdir($home, "pcm");
} else {
    if ($> == 0) {  # root
        $g_work_dir = File::Spec->catdir(File::Spec->rootdir(), qw/etc pcm/);
        $g_lock_dir = File::Spec->catdir(File::Spec->rootdir(), qw/var lock pcm/);
    } else {        # normal user
        die "Can't find HOME environment variable!\n" if !exists $ENV{HOME};

        $g_work_dir = File::Spec->catdir($ENV{HOME}, ".pcm");
    }
sub new {
    my ($class, %args) = @_;

    bless $self, $class;
}


