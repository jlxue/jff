use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    dist_name           => 'MarkIt',
    module_name         => 'MarkIt',
    dist_abstract       => 'Abstract goes here.',
    dist_author         => 'Liu Yubao <Yubao.Liu@gmail.com>',
    license             => 'gpl3',
    dist_version        => '1.0',
    tmpl_files          => eval {
        my %tmpls = ();
        my $tmpl_dir = 'lib/MarkIt/templates';
        File::Find::find(
            {
                wanted => sub {
                    if ( -f $_ ) {
                        $tmpls{$File::Find::name} = $File::Find::name;
                    }
                },
            },
            $tmpl_dir
        ) if -d $tmpl_dir;
        return \%tmpls;
    },
    build_requires => {
        'Test::More'                    => 0,
        'Test::WWW::Mechanize::CGIApp'  => 0,
    },
    requires => {
        'Titanium'                      => 0,
        'HTML::Template'                => 0,
        'CGI::Application::Server'      => 0,
        'GD::SecurityImage'             => 0,
    },
    add_to_cleanup      => [ 'MarkIt-*' ],
    create_makefile_pl  => 'traditional',
    sign                => 1,
);

$builder->add_build_element('tmpl');
$builder->create_build_script();

