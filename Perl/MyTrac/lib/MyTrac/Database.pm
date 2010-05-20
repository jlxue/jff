package MyTrac::Database;
use Any::Moose;
use Digest::SHA1 qw/sha1_hex/;
use Fcntl qw/:flock/;
use File::Spec;
use File::Temp;
use Guard;      # or Scope::Guard?
use JSON 2.0 qw//;
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

    if (! -e File::Spec->catdir($self->git_dir)) {
        my @dirs = File::Spec->splitdir($self->git_dir);
        pop @dirs;
        my $repos = File::Spec->catdir(@dirs);

        confess "Can't initialize database!" if
                0 != system(qw/git init -q/, $repos);
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

sub sync_transact {
    my ($self, $author, $message, $func, @args) = @_;

    confess "Transaction can't be nested!" if defined $self->operations;
    $self->operations([]);
    $self->async(0);

    scope_guard {
        # Destructors of database operations will do cleanup.
        $self->operations(undef);
    };

    my $result = $func->(@args);

    return $result if ! defined $self->operations;

    my $operations = $self->operations;

    # they are successful
    for my $op (@$operations) {
        $op->commit();
    }

    return $result;
}

sub async_transact {
    my ($self, $author, $message, $func, @args) = @_;

    confess "Transaction can't be nested!" if defined $self->operations;
    $self->operations([]);
    $self->async(1);

    scope_guard {
        # Destructors of database operations will do cleanup.
        $self->operations(undef);
    };

    my $result = $func->(@args);

    return $result if ! defined $self->operations;

    my $operations = $self->operations;

    # acquire all required resources for this transaction.
    for my $op (@$operations) {
        $op->prepare();
    }

    # do jobs
    for my $op (@$operations) {
        $op->execute();
    }

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
}

sub update {
    my ($self, $obj) = @_;

    confess "Transaction not started!" if !defined $self->operations;
}

sub delete {
    my ($self, $id) = @_;

    confess "Transaction not started!" if !defined $self->operations;
}

sub select {
    my ($self, $id) = @_;

    confess "Transaction not started!" if !defined $self->operations;
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

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
