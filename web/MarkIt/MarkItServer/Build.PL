use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    dist_name           => 'MarkItServer',
    module_name         => 'MarkItServer',
    dist_abstract       => 'Abstract goes here.',
    dist_author         => 'Liu Yubao <Yubao.Liu@gmail.com>',
    license             => 'GPL v3',
    dist_version        => '0.01',
    tmpl_files          => eval {
        my %tmpls; File::Find::find(
            {
                wanted => sub {
                    if ( -f $_ ) {
                        $tmpls{$File::Find::name} = $File::Find::name;
                    }
                },
            },
            'lib/MarkItServer/templates',
        );
        return \%tmpls;
    },
    build_requires => {
        'Test::More'                    => 0,
        'Test::WWW::Mechanize::CGIApp'  => 0,
    },
    requires => {
        'Titanium'                      => 0,
        'HTML::Template'                => 0,
    },
    add_to_cleanup      => [ 'MarkItServer-*' ],
    create_makefile_pl  => 'traditional',
    sign                => 1,
);

$builder->add_build_element('tmpl');
$builder->create_build_script();

