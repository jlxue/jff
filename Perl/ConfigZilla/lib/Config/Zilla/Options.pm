package Config::Zilla::Options;
use strict;
use warnings;
use utf8;
use Any::Moose;
use File::Spec;
use constant CUR_DIR    => File::Spec->curdir;

use Config::Zilla::Types;


has 'validate_mode' => (is => 'rw', isa => 'Bool', default => 0);
has 'dryrun_mode'   => (is => 'rw', isa => 'Bool', default => 0);
has 'maxtime'       => (is => 'rw', isa => 'NonNegativeInt', default => 1800);
has 'concurrent'    => (is => 'rw', isa => 'NonNegativeInt', default => 5);
has 'lock_dir'      => (is => 'rw', isa => 'Str', default => CUR_DIR);
has 'state_dir'     => (is => 'rw', isa => 'Str', default => CUR_DIR);
has 'backup_dir'    => (is => 'rw', isa => 'Str', default => CUR_DIR);

sub build_lock_file {
    confess "Bad argument!" unless @_ == 2;
    File::Spec->catfile($_[0]->lock_dir, $_[1]);
}

sub build_state_file {
    confess "Bad argument!" unless @_ == 2;
    File::Spec->catfile($_[0]->state_dir, $_[1]);
}

sub build_backup_file {
    confess "Bad argument!" unless @_ == 2;
    File::Spec->catfile($_[0]->backup_dir, $_[1]);
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
