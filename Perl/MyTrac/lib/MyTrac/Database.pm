package MyTrac::Database;
use Any::Moose;
use Digest::SHA1 qw/sha1_hex/;
use Fcntl qw/:DEFAULT :flock/;
use File::Spec;
use File::Temp;
use Guard;      # or Scope::Guard?
use JSON 2.0 qw//;
use MyTrac::Database::DeleteOp;
use MyTrac::Database::InsertOp;
use MyTrac::Database::SelectOp;
use MyTrac::Database::UpdateOp;
use namespace::autoclean;
#use Smart::Comments;

our $VERSION = '0.01';

has 'git_dir'   => (is => 'ro', isa => 'Str', required => 1);
has 'work_tree' => (is => 'ro', isa => 'Str', required => 1);
has 'json'      => (is => 'rw', isa => 'Ref');
has 'operations'=> (is => 'rw', isa => 'ArrayRef[MyTrac::Database::Operation]');
has 'async'     => (is => 'rw', isa => 'Bool', default => 1);

sub BUILD {
    my ($self, $args) = @_;

    $self->json(JSON->new->utf8->canonical->pretty->relaxed);

    if (! -e $self->git_dir) {
        my @dirs = File::Spec->splitdir($self->git_dir);
        pop @dirs;
        my $repos = File::Spec->catdir(@dirs);

        confess "Can't initialize database!" if
                0 != system(qw/git init -q/, $repos);
    }

    my $lockfile = $self->git_lockfile_path;
    if (! -e $lockfile) {
        sysopen my $fh, $lockfile, O_WRONLY | O_CREAT, 0644;
        confess "Can't create lock file $lockfile: $!" if ! defined $fh;
        close $fh;
    }
}

sub pack {
    my ($self, $obj) = @_;

    my $h = {};
    $h->{__CLASS__} = $obj->meta->name . "-" . UNIVERSAL::VERSION($obj);
    my @attributes = $obj->meta->get_all_attributes;

    for my $attr (@attributes) {
        if ($attr->has_value($obj)) {
            $h->{$attr->name} = $attr->get_value($obj);
        }
    }

    return $h;
}

sub unpack {
    my ($self, $h, $class) = @_;

    if (exists $h->{__CLASS__}) {
        my ($c, $v) = split /-/, $h->{__CLASS__}, 2;

        confess "Bad class signature!" if (! defined $v);
        confess "Versions don't match!" if $v ne UNIVERSAL::VERSION($c);

        if (defined $class) {
            confess "Classes don't match!" if $class ne $c;
        } else {
            $class = $c;
        }

        delete $h->{__CLASS__};
    } elsif (!defined $class) {
        confess "Class not specified!";
    }

    $class->new($h);
}

sub to_json {
    my ($self, $perl) = @_;

    $self->json->encode($perl);
}

sub from_json {
    my ($self, $json) = @_;

    $self->json->decode($json);
}

sub transact($\%&@) {
    my ($self, $options, $func, @args) = @_;
    my $lock_fh;

    confess "Transaction can't be nested!" if defined $self->operations;
    $self->operations([]);

    if (exists $options->{async} && $options->{async}) {
        $self->async(1);
    } else {
        $self->async(0);
    }

    scope_guard {
        # Destructors of database operations will do cleanup.
        $self->operations(undef);

        close $lock_fh if defined $lock_fh;
    };

    my $result = $func->(@args);

    return $result if ! defined $self->operations;

    my $operations = $self->operations;

    if ($self->async) {
        # acquire all required resources for this transaction.
        for my $op (@$operations) {
            $op->prepare();
        }

        # do jobs
        for my $op (@$operations) {
            $op->execute();
        }
    }

    sysopen $lock_fh, $self->git_lockfile_path, O_WRONLY;
    confess "Can't open lock file for transaction: $!" if !defined $lock_fh;
    flock($lock_fh, LOCK_EX) or confess "Can't lock for transaction: $!";

    # commit this transaction
    my @insert_files = ();
    my @update_files = ();
    my @delete_files = ();
    for my $op (@$operations) {
        if ($op->isa('MyTrac::Database::InsertOp')) {
            push @insert_files, $op->filename;
        } elsif ($op->isa('MyTrac::Database::UpdateOp')) {
            push @update_files, $op->filename;
        } elsif ($op->isa('MyTrac::Database::DeleteOp')) {
            push @delete_files, $op->filename;
        } else {
            Carp::cluck("This kind operation shouldn't be added into transaction!");
        }
    }
    my @files = (@insert_files, @update_files);
    if (@files > 0) {
        confess "Failed to git-add files: @files" if
                0 != system($self->git_cmd('add', @files));
    }
    if (@delete_files > 0) {
        confess "Failed to git-rm files: @delete_files" if
                0 != system($self->git_cmd(qw/rm -f --cached --/, @delete_files));
    }

    push @files, @delete_files;
    my @commit_options = ();
    push @commit_options, '--author=' . $options->{author} if exists $options->{author};
    push @commit_options, '-m', $options->{shortlog} if exists $options->{shortlog};
    push @commit_options, '-m', $options->{log} if exists $options->{log};
    push @commit_options, '--';
    confess "Failed to git-commit files: @files" if
            0 != system($self->git_cmd(qw/commit -q/, @commit_options, @files));

    # they are successful
    for my $op (@$operations) {
        $op->commit();
    }

    return $result;
}

sub insert {
    my ($self, $obj) = @_;

    confess "Transaction not started!" if !defined $self->operations;

    my $h = $self->pack($obj);
    delete $h->{qw/id revision/};

    my $json = $self->to_json($h);

    $obj->id(sha1_hex($json));

    my $op = new MyTrac::Database::InsertOp({
            db => $self,
            id => $obj->id,
            data => $json,
        });

    push @{$self->operations}, $op;

    if (! $self->async) {
        $op->prepare;
        $op->execute;
    }
}

sub update {
    my ($self, $obj) = @_;

    confess "Transaction not started!" if !defined $self->operations;

    my $h = $self->pack($obj);
    delete $h->{revision};

    my $json = $self->to_json($h);

    my $op = new MyTrac::Database::UpdateOp({
            db => $self,
            id => $obj->id,
            data => $json,
        });

    push @{$self->operations}, $op;

    if (! $self->async) {
        $op->prepare;
        $op->execute;
    }
}

sub delete {
    my ($self, $id) = @_;

    confess "Transaction not started!" if !defined $self->operations;

    my $op = new MyTrac::Database::DeleteOp({
            db => $self,
            id => $id,
        });

    push @{$self->operations}, $op;

    if (! $self->async) {
        $op->prepare;
        $op->execute;
    }
}

sub select {
    my ($self, $id, $revision) = @_;

    confess "Transaction not started!" if !defined $self->operations;

    my %args = { db => $self, id => $id };
    $args{revision} = $revision if defined $revision;

    my $op = new MyTrac::Database::SelectOp(\%args);

    $op->prepare;
    my $json = $op->execute;
    $op->commit;

    my $obj = $self->unpack($self->from_json($json));
    $obj->revision($revision) if defined $revision;

    return $obj;
}

sub git_cmd {
    my ($self, @args) = @_;

    return ('git', '--git-dir=' . $self->git_dir,
            '--work_tree=' . $self->work_tree, @args);
}

sub git_path {
    my ($self, $filename) = @_;

    return File::Spec->catfile($self->work_tree, $filename);
}

sub git_lockfile_path {
    return File::Spec->catfile($_[0]->git_dir, 'mytrac.lck');
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
