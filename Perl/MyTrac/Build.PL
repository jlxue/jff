use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    dist_name           => 'MyTrac',
    module_name         => 'MyTrac',
    dist_abstract       => 'A project management system like bulletin board system',
    dist_author         => 'Liu Yubao <Yubao.Liu@gmail.com>',
    license             => 'perl',
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
            'lib/MyTrac/templates',
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
        'Moose'                         => 0,
        'MooseX::Storage'               => 0,
        'MooseX::Storage::Format::JSONpm'   => 0,
    },
    add_to_cleanup      => [ 'MyTrac-*' ],
    create_makefile_pl  => 'traditional',
    sign                => 1,
);

$builder->add_build_element('tmpl');
$builder->create_build_script();

