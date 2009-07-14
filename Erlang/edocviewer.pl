#!/usr/bin/perl -w
use strict;
use warnings;
use Data::Dumper;
use IO::Dir;
use Tk;

# {
#  'appA-vsn' => {'moduleA' => ['moduleA_href', {
#                               'funcA' => 'doc...',
#                               'funcB' => 'doc...'
#                              }],
#                 'moduleB' => ['moduleB_href', {
#                               'funcC' => 'doc...',
#                               'funcB' => 'doc...'
#                              }]
#                }
# }
#
my $erl_apps = generate_apps($ARGV[0]);
my ($app_filter, $module_filter, $function_filter) = ("", "", "");

my $mw = new MainWindow();

###
my $app_frame = $mw->LabFrame(-label => "Applications",
                              -labelside => "acrosstop");
my $app_filter_labentry = $app_frame->LabEntry(-label => "Filter:",
                                              -textvariable => \$app_filter)->pack;
my $app_listbox = $app_frame->Scrolled(qw/Listbox -width 20 -height 40
                                          -setgrid 1 -scrollbars se/);
$app_listbox->pack(qw/-expand yes -fill y/);
$app_listbox->focus;
$app_listbox->insert(0, sort keys %$erl_apps);
$app_listbox->activate(0);
$app_listbox->bind('<<ListboxSelect>>', \&on_app_listbox_selchanged);

###
my $module_frame = $mw->LabFrame(-label => "Modules",
                                 -labelside => "acrosstop");
my $module_filter_labentry = $module_frame->LabEntry(-label => "Filter:",
                                                     -textvariable => \$module_filter)->pack;
my $module_listbox = $module_frame->Scrolled(qw/Listbox -width 20 -height 40
                                                -setgrid 1 -scrollbars se/);
$module_listbox->pack(qw/-expand yes -fill y/);
$module_listbox->insert(0, sort keys %{$erl_apps->{$app_listbox->get(0)}});
$module_listbox->activate(0);
$module_listbox->bind('<<ListboxSelect>>', \&on_module_listbox_selchanged);

###
my $function_frame = $mw->LabFrame(-label => "Functions",
                                   -labelside => "acrosstop");
my $function_filter_labentry = $function_frame->LabEntry(-label => "Filter:",
                                                         -textvariable => \$function_filter)->pack;
my $function_listbox = $function_frame->Scrolled(qw/Listbox -width 20 -height 40
                                                    -setgrid 1 -scrollbars se/);
$function_listbox->pack(qw/-expand yes -fill y/);
$function_listbox->insert(0, sort keys %{$erl_apps->{$app_listbox->get(0)}{$module_listbox->get(0)}[1]});
$function_listbox->activate(0);
$function_listbox->bind('<<ListboxSelect>>', \&on_function_listbox_selchanged);

###
my $viewer_frame = $mw->LabFrame(-label => "Description",
                                 -labelside => "acrosstop");
my $viewer_textbox = $viewer_frame->Scrolled(qw/Text -width 60 -height 40
                                                -setgrid 1 -scrollbars se/);
$viewer_textbox->pack(qw/-expand yes -fill y/)->pack;

###
$app_frame->pack(qw/-side left -expand yes -fill y/);
$module_frame->pack(qw/-side left -expand yes -fill y/);
$function_frame->pack(qw/-side left -expand yes -fill y/);
$viewer_frame->pack(qw/-side left -expand yes -fill y/);

MainLoop();


sub on_app_listbox_selchanged {
    $module_listbox->delete(0, 'end');
    $module_listbox->insert(0, sort keys %{$erl_apps->{$app_listbox->get('anchor')}});
    $module_listbox->activate(0);

    on_module_listbox_selchanged($module_listbox);
}

sub on_module_listbox_selchanged {
    $function_listbox->delete(0, 'end');
    $function_listbox->insert(0, sort keys %{$erl_apps->{$app_listbox->get('anchor')}{$module_listbox->get('active')}[1]});
    $function_listbox->activate(0);
}

sub on_function_listbox_selchanged {
}


# generate_apps("e:/erl5.6.5/lib")
#
sub generate_apps {
    my ($lib_root) = @_;
    my $apps = {};

    my $d = new IO::Dir($lib_root);
    die "Can't open dir $lib_root: $!\n" if ! defined $d;
    while (defined(my $app = $d->read)) {
        next if $app eq File::Spec->curdir() or
                $app eq File::Spec->updir();
        my ($name) = $app =~ /^(\w+)/;
        $apps->{$app} = generate_modules($lib_root, $app)
            if -f File::Spec->catfile($lib_root, $app,
                                      'ebin',
                                      $name . '.app');
    }
    undef $d;

    return $apps;
}

# generate_modules("e:/erl5.6.5/lib", "kernel-2.12.5")
#
sub generate_modules {
    my ($lib_root, $app) = @_;
    my $modules = {};

    my $html = File::Spec->catfile($lib_root, $app,
                                   'doc', 'html',
                                   'ref_man.html');
    $html = File::Spec->catfile($lib_root, $app,
                                   'doc', 'html',
                                   'refman.html')
        if ! -f $html;

    return {} if ! -f $html;

    open my $fh, $html or die "Can't open $html: $!\n";
    while (<$fh>) {
        if (/^<small><a target="document" href="([^\.>]+)\.html">([^<]+)/) {
            my ($href, $name) = ($1, $2);
            $name =~ s/[^[:alnum:]_]//g;
            $modules->{$name} = [$href, {}];
        }
    }
    undef $fh;

    my $kwc = $html;
    $kwc =~ s/\.html$/\.kwc/;
    parse_kwc_file($modules, $kwc);

    return $modules;
}

# This is faster than parsing Module-Name.html but not complete.
#
sub parse_kwc_file {
    my ($modules, $kwc) = @_;
    my $module = "";
    my @functions = ();

    open my $fh, $kwc or die "Can't open $kwc: $!\n";
    while (<$fh>) {
        if (/^{module, "(.*)"}\.$/) {
            $module = $1;
        } elsif (/^{name, "(.*)"}\.$/) {
            push @functions, $1;
        } elsif (/^{fsummary, "(.*)"}\.$/) {
            for (@functions) {
                warn "$module in $kwc doesn't exist in ref_man.html!\n"
                    if ! exists $modules->{$module};
                $modules->{$module}[1]{$_} = $1;
            }
            @functions = ();
        }
    }
    undef $fh;
}

# generate_functions("e:/erl5.6.5/lib", "kernel-2.12.5", "code")
#
#
sub generate_functions {
    my ($lib_root, $app, $module) = @_;
    my $functions = {};

    my $html = File::Spec->catfile($lib_root, $app,
                                   'doc', 'html',
                                   $module . '.html');
    open my $fh, $html or die "Can't open $html: $!\n";
    while (<$fh>) {
    }
    undef $fh;

    return $functions;
}

