package Config::Zilla::Engine;
use strict;
use warnings;
use utf8;
use Any::Moose;
use Carp qw/cluck/;
use Data::Dumper;
use Digest::MD5 qw/md5_hex/;
use File::Spec;
use IO::File;
use List::Util qw/max min sum/;
use Log::Log4perl qw/:easy/;
use Module::Load;
use POE qw/Wheel::Run/;
use POSIX ();
use Try::Tiny;
use YAML::Tiny;

use Config::Zilla::ExecutorState;
use Config::Zilla::FileLock;
use Config::Zilla::Options;
use Config::Zilla::Rule;
use Config::Zilla::RuleExecutor;


has 'ruleset'       => (is => 'ro', isa => 'HashRef[Config::Zilla::Rule]',
                        default => sub { {} });

sub addRule {
    my ($self, $rule) = @_;

    LOGCONFESS "Invalid argument" unless blessed $rule &&
        $rule->isa('Config::Zilla::Rule');

    my $ruleset = $self->ruleset;
    my $name = $rule->name;

    if (exists $ruleset->{$name}) {
        my $r = $ruleset->{$name};

        LOGCONFESS "Conflict rule names: $name ($r->shortdesc)";
    }

    $ruleset->{$name} = $rule;

    if (get_logger()->is_debug) {
        DEBUG "Added rule $name, depends=",
            @{$rule->depends}, ", maxtime=", $rule->maxtime;
    }
}


sub run {
    my ($self, $options) = @_;
    my $ruleset = $self->ruleset;

    $options = Config::Zilla::Options->new unless defined $options;
    LOGCONFESS "Invalid argument" unless blessed $options &&
        $options->isa('Config::Zilla::Options');


    _validate($ruleset);


    my $executors = _resolve_executors($ruleset);

    # _resolve_executors() also contains necessary validation
    if ($options->validate_mode) {
        INFO "Validate only, not really run engine";
        return 1;
    }


    POE::Session->create(
        inline_states => {
            _start  => \&_on_start,
            _stop   => \&_on_stop,
            timeout => \&_on_timeout,

            child_stdout    => \&_on_child_stdout,
            child_stderr    => \&_on_child_stderr,
            child_exit      => \&_on_child_exit,
            child_timeout   => \&_on_child_timeout,
        },

        args => [$ruleset, $executors, $options],
    );

    POE::Kernel->run;

    return 1;
}


sub _validate {
    my ($ruleset) = @_;

    INFO "Validating rules...";

    _validate_dependent_rules_exist($ruleset);

    _validate_circular_dependency($ruleset);
}


sub _validate_dependent_rules_exist {
    my ($ruleset) = @_;

    while (my ($name, $rule) = each %$ruleset) {
        my $depends = $rule->depends;

        for my $dep (@$depends) {
            LOGCONFESS "Rule \"$name\" depends on non-exist rule \"$dep\""
                unless exists $ruleset->{$dep};
        }
    }
}


sub _validate_circular_dependency {
    my ($ruleset) = @_;
    my ($rulegraph, $got);

    $rulegraph = _construct_rule_graph($ruleset);

    while (keys(%$rulegraph) > 0) {
        $got = 0;

        for my $name (keys %$rulegraph) {
            if (keys %{ $rulegraph->{$name} } == 0) {
                $got = 1;

                delete $rulegraph->{$name};

                map { delete $_->{$name} } values %$rulegraph;
            }
        }

        if (! $got && keys(%$rulegraph) > 0) {
            LOGCONFESS "Found circular dependency:\n" .
                    Data::Dumper->Dump([$rulegraph], [qw/rulegraph/]);
        }
    }
}


sub _construct_rule_graph {
    my ($ruleset) = @_;
    my %g;

    while (my ($name, $rule) = each %$ruleset) {
        $g{$name} = {};

        my $depends = $rule->depends;

        for my $dep (@$depends) {
            $g{$name}->{$dep} = 1;
        }
    }

    return \%g;
}


sub _resolve_executors {
    my ($ruleset) = @_;
    my %executors;

    INFO "Resolving executors...";

    while (my ($name, $rule) = each %$ruleset) {
        my $executor = $rule->executor;
        # XXX: is it better to call ref() to get the class name?
        $executor = $rule->meta->name . "Executor" unless defined $executor;

        load $executor;
        $executors{$name} = $executor->new(rule => $rule);
    }

    return \%executors;
}


sub _on_start {
    my ($kernel, $heap, $ruleset, $executors, $options) =
        @_[KERNEL, HEAP, ARG0, ARG1, ARG2];

    INFO "On engine start, ", scalar(keys %$ruleset), " rules to execute";

    $heap->{ruleset} = $ruleset;
    $heap->{rulegraph} = _construct_rule_graph($ruleset);
    $heap->{executors} = $executors;
    $heap->{options} = $options;
    $heap->{states} = {};
    $heap->{children_by_wid} = {};
    $heap->{children_by_pid} = {};
    $heap->{pid_to_name} = {};
    $heap->{pid_to_timer} = {};


    if ($options->concurrent > 0) {
        INFO "Allow at most ", $options->concurrent, " concurrent executors";
    } else {
        INFO "Allow unlimited concurrent executors";
    }

    # pass 0 to sum() to avoid returning undef when no rule is specified
    $options->maxtime(max($options->maxtime,
                          sum(0, map { $_->maxtime } values %$ruleset)));


    if ($options->maxtime > 0) {
        INFO "Schedule engine timeout timer to ", $options->maxtime, " seconds later";
        $kernel->delay("timeout", $options->maxtime);
    }


    _run_executors($kernel, $heap);
}


sub _on_stop {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    INFO "On engine stop";
}


sub _on_timeout {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    WARN "On engine timeout";

    $kernel->alarm_remove_all;
}


sub _on_child_stdout {
    my ($heap, $line, $wheel_id) = @_[HEAP, ARG0, ARG1];
    my $child = $heap->{children_by_wid}{$wheel_id};
    my $pid = $child->PID;
    my $name = $heap->{pid_to_name}{$pid};

    INFO "$name($pid): $line\n";
}


sub _on_child_stderr {
    my ($heap, $line, $wheel_id) = @_[HEAP, ARG0, ARG1];
    my $child = $heap->{children_by_wid}{$wheel_id};
    my $pid = $child->PID;
    my $name = $heap->{pid_to_name}{$pid};

    ERROR "$name($pid): $line\n";
}


sub _on_child_timeout {
    my ($kernel, $heap, $child) = @_[KERNEL, HEAP, ARG0];
    my $pid = $child->PID;
    my $name = $heap->{pid_to_name}{$pid};

    WARN "On child timeout: $name($pid)";

    if ($child->kill) {
        # TODO:
    }

    delete $heap->{pid_to_timer}{$child->PID};
}


sub _on_child_exit {
    my ($kernel, $heap, $sig, $pid, $exit_code) =
        @_[KERNEL, HEAP, ARG0, ARG1, ARG2];
    my $child = $heap->{children_by_pid}{$pid};
    my $name = $heap->{pid_to_name}{$pid};
    my $state = $heap->{states}{$name};

    INFO "On child exit: $name($pid) exit_code=$exit_code";

    delete $heap->{children_by_wid}{$child->ID};
    delete $heap->{children_by_pid}{$pid};
    delete $heap->{pid_to_name}{$pid};

    if (exists $heap->{pid_to_timer}{$pid}) {
        $kernel->alarm_remove($heap->{pid_to_timer}{$pid});
        delete $heap->{pid_to_timer}{$pid};
    }

    $state->end_time(time());
    $state->exit_code($exit_code);

    _remove_rule_from_graph($name, $heap->{rulegraph});

    _run_executors($kernel, $heap);
}


sub _run_executors {
    my ($kernel, $heap) = @_;

    my $options = $heap->{options};
    my $ruleset = $heap->{ruleset};
    my $executors = $heap->{executors};
    my $states = $heap->{states};
    my $rulegraph = $heap->{rulegraph};

    my $unapplied_count = keys %$rulegraph;
    my $max_count = $unapplied_count;
    my $running_count = keys %{ $heap->{children_by_pid} };
    my $newly_count = 0;

    if ($options->concurrent > 0) {
        $max_count = min($options->concurrent - $running_count, $max_count);
    }

    INFO "$max_count executors to be run, ",
        "$running_count running executors, ",
        "$unapplied_count unapplied rules";

    while ($max_count > 0 && keys(%$rulegraph) > 0) {
        my $got = 0;

        for my $name (keys %$rulegraph) {
            ## topological sort, whether this node has no predecessor
            next unless keys %{ $rulegraph->{$name} } == 0;
            ++$got;
            ++$newly_count;

            $states->{$name} = Config::Zilla::ExecutorState->new;

            my $rule = $ruleset->{$name};
            my %exit_codes;
            for my $dep (@{ $rule->depends }) {
                $exit_codes{$dep} = $states->{$dep}->exit_code;
            }

            my $child = POE::Wheel::Run->new(
                Program     => \&_run_executor_in_child_process,
                ProgramArgs => [ $executors->{$name}, $options, \%exit_codes ],
                StdoutEvent => "child_stdout",
                StderrEvent => "child_stderr",
            );

            $kernel->sig_child($child->PID, "child_exit");
            $heap->{children_by_wid}{$child->ID} = $child;
            $heap->{children_by_pid}{$child->PID} = $child;
            $heap->{pid_to_name}{$child->PID} = $name;

            INFO "Created child ", $child->PID, " for rule $name";

            if ($rule->maxtime > 0) {
                INFO "Set child timeout ", $rule->maxtime,
                    " seconds for $name(", $child->PID, ")";

                $heap->{pid_to_timer}{$child->PID} =
                    $kernel->delay_set("child_timeout",
                                       $rule->maxtime,
                                       $child);
            }

            last if --$max_count == 0;
        }

        last if $got == 0;
    }

    if (keys(%$rulegraph) == 0 && keys(%{ $heap->{children_by_pid} }) == 0) {
        INFO "No job left, clear engine timeout";
        $kernel->delay("timeout");
    }

    return $newly_count;
}


sub _run_executor_in_child_process {
    my ($executor, $options, $exit_codes) = @_;
    my $rule = $executor->rule;
    my $name = $rule->name;

    untie *STDIN;

    my $nullfh = IO::File->new(File::Spec->devnull);
    open STDIN, '<&', $nullfh->fileno or LOGCONFESS "Can't redirect stdin($$): $!";

    my $mutex;
    my $exit_code = 0;

    #
    # exclusively lock
    #
    if (! $options->dryrun_mode) {
        try {
            INFO "Locking for rule $name($$)...";

            my $lock_file = $options->build_lock_file("$name.lck");

            my $lock = Config::Zilla::FileLock->new($lock_file,
                "Exclusive file lock for Config-Zilla to " .
                    "avoid multiple instances applying " .
                    "same rule concurrently.");

            if ($lock->locked) {
                $mutex = $lock;
            } else {
                if ($!{EEXIST}) {
                    WARN "Another czil instance is applying rule ",
                        "$name($$) or $lock_file is stale";
                }

                $exit_code = 1;
            }
        } catch {
            ERROR "Caught exception when lock rule $name($$): $_";
            $exit_code = 1;
        };
    }


    #
    # rollback if necessary, maybe last time we were interruptted
    #
    my $state_file = $options->build_state_file("$name.yml");
    if (! $options->dryrun_mode && $exit_code == 0) {
        if (-e $state_file) {
            WARN "Find state file $state_file, rollback for rule $name($$)...";

            try {
                my $content;

                {
                    open my $fh, $state_file or die "Can't read $state_file: $!";
                    binmode $fh;
                    local $/;
                    $content = <$fh>;
                    close $fh;
                }

                my ($len, $md5) = $content =~ /^# len=(\d+) md5=(\S+)/;
                die "Bad state file $state_file!" unless defined $md5;
                $content = substr($content, index($content, "\n---\n") + 1);
                die "Mismatch length in state file $state_file!" unless $len == length($content);
                die "Mismatch md5sum in state file $state_file!" unless $md5 eq md5_hex($content);

                my $state = YAML::Tiny::Load($content);
                die "Invalid state file $state_file!" unless defined $state;

                $executor->state($state);
                if (! $executor->rollback()) {
                    ERROR "Failed to rollback for rule $name($$)";
                    $exit_code = 2;
                } else {
                    $executor->state(undef);
                    unlink $state_file or die "Can't delete $state_file: $!";
                }
            } catch {
                ERROR "Caught exception when rollback rule $name($$): $_";
                $exit_code = 2;
            };
        }
    }


    #
    # prepare
    #
    if ($exit_code == 0) {
        try {
            INFO "Preparing for rule $name($$)...";
            if (! $executor->prepare($options, $exit_codes)) {
                ERROR "Failed to prepare for rule $name($$)";
                $exit_code = 3;
            } else {
                if (! $options->dryrun_mode && defined $executor->state) {
                    my $content = YAML::Tiny::Dump($executor->state);
                    my $len = length($content);
                    my $md5sum = md5_hex($content);

                    open my $fh, ">", $state_file or die "Can't write $state_file: $!";
                    binmode $fh;
                    defined(syswrite $fh, "# len=$len md5=$md5sum\n") or
                        die "Error occurred when write $state_file: $!";
                    defined(syswrite $fh, $content) or
                        die "Error occurred when write $state_file: $!";
                    close $fh;
                }
            }
        } catch {
            ERROR "Caught exception when prepare rule $name($$): $_";
            if (-e $state_file) {
                unlink $state_file or ERROR "Can't delete $state_file: $!";
            }
            $exit_code = 3;
        };
    }


    #
    # execute
    #
    if ($exit_code == 0) {
        try {
            INFO "Executing for rule $name($$)...";
            if (! $executor->execute($options)) {
                ERROR "Failed to execute rule $name($$)";
                $exit_code = 4;
            }
        } catch {
            ERROR "Caught exception when execute rule $name($$): $_";
            $exit_code = 4;
        };
    }

    #
    # rollback if necessary
    #
    if (! $options->dryrun_mode && $exit_code == 4) {
        try {
        } catch {
        };
    }

    POSIX::_exit($exit_code);
}


sub _remove_rule_from_graph {
    my ($name, $rulegraph) = @_;

    delete $rulegraph->{$name};
    map { delete $_->{$name} } values %$rulegraph;
}


no Any::Moose;
__PACKAGE__->meta->make_immutable();
