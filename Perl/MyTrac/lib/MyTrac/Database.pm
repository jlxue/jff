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
has 'operations'=> (is => 'rw', isa => 'Undef | ArrayRef[MyTrac::Database::Operation]');
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
        sysopen my $fh, $lockfile, O_WRONLY | O_CREAT, 0644 or
                confess "Can't create lock file $lockfile: $!";
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
    confess "\"author\" not specified!" if ! exists $options->{author};
    confess "\"shortlog\" or \"log\" not specified!" if ! exists $options->{shortlog} && ! exists $options->{log};

    $self->operations([]);

    if (exists $options->{sync} && $options->{sync}) {
        $self->async(0);
    } else {
        $self->async(1);
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

    # lock git database
    sysopen $lock_fh, $self->git_lockfile_path, O_WRONLY or
            confess "Can't open lock file for transaction: $!";
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

    my @commit_options = ();
    push @commit_options, '--author=' . $options->{author} if exists $options->{author};
    push @commit_options, '-m', $options->{shortlog} if exists $options->{shortlog};
    push @commit_options, '-m', $options->{log} if exists $options->{log};
    confess "Failed to git-commit files: @files" if
            0 != system($self->git_cmd(qw/commit -q/, @commit_options));

    # they are successful
    for my $op (@$operations) {
        $op->commit();
    }

    return $result;
}

sub insert {
    my ($self, $obj) = @_;

    confess "Transaction not started!" if !defined $self->operations;

    $obj->timestamp(time());

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

    $obj->timestamp(time());

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
            '--work-tree=' . $self->work_tree, @args);
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

__END__

=pod

=encoding utf8

=head1 The design of the document database using GIT as backend

文件根据内容计算出 sha1 摘要，以此作为文件名。
读写 git 工作目录中的文件应该允许最大的并行性，不要做全局锁。
改写 .git/ 内容时可以做一个全局锁(.git/mytrac.lck)。

=head2 transact()

允许多个数据库操作在一个事务环境下执行，部分失败回导致操作被撤销。
事务不允许嵌套。

事务的执行分成同步和异步两种方式，同步指请求一个操作马上改写 git
工作目录里的文件，最后统一提交； 异步指请求一个非 select 的操作时
只是记录操作，等最后统一执行并提交。

由于事务中要对所有涉及的文件加锁，所以异步方式可以最大程度避免
某个锁获取失败后有些操作进行了一半。

    transact({...options...}, \&subroutine, @args)

options 有如下值：
  author => 'XXX <YYY>'，作为 git commit 的 --author 参数。
  shortlog => '...', 作为 git commit 的 -m 参数。
  log => '...', 作为 git commit 的 -m 参数。
  sync => 1, 事务以同步方式执行。

  transact() 依赖 Guard 或者 Scope::Guard 模块，确保在 transact()
  退出前执行一段代码， 这段代码会取消对所有 Operation 实例的引用，
  导致触发它们的析构函数。


=head2 Database::Operation

=head3 prepare()

打开文件、获取锁。

=head3 execute()

改写工作目录中的文件，注意不释放锁。

=head3 rollback()

撤销操作。

=head3 commit()

标记操作成功。

=head3 DEMOLISH()

根据是否调用过 commit() 决定是否撤销操作，注意在这里才释放锁。


=head2 Database::InsertOp

互斥方式创建文件，加互斥锁，成功后确保文件大小是零，以免在打开和加锁过程中文件被改写。

=head2 Database::DeleteOp

打开文件加互斥锁。 不能在事务成功前直接删除文件，因为此事务失败时还需恢复此文件。

在事务成功后可以删除此文件。

=head2 Database::UpdateOp

打开文件加互斥锁，改写文件。

=head2 Database::SelectOp

打开文件加共享锁，读取文件。

=cut

