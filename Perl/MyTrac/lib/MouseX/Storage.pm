package MouseX::Storage;
use Mouse;
use namespace::autoclean;

our $VERSION = '0.01';

sub import {
    my $pkg = caller();

    return if $pkg eq 'main';

    ($pkg->can('meta')) || confess "This package can only be used in Moose based classes";

    $pkg->meta->add_method('Storage' => sub {
            return ('MouseX::Storage::Basic');
        });
}

no Mouse;
__PACKAGE__->meta->make_immutable();
1;
