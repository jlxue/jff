package NewSMTH::DB;

use DBIx::Class::Loader;

sub connect {
    my $loader = DBIx::Class::Loader->new(
        dsn         => 'dbi:mysql:newsmth',
        user        => 'root',
        password    => '',
        namespace   => 'NewSMTH');
}


1;

