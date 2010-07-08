package MarkIt::User;
use GD::SecurityImage;
use base 'MarkIt::Base';

our $VERSION = '0.01';


sub setup {
    my ($c) = @_;

    $c->start_mode('create');
    $c->run_modes([qw/create/]);
}


sub create {
    my ($c) = @_;

    my $image = GD::SecurityImage->new(width    => 80,
                                       height   => 30,
                                       lines    => 10,
                                       scramble => 1,
                                       gd_font  => 'giant');
    $image->random();
    $image->create("normal", "default");
    $image->particle();

    my ($image_data, $mime_type, $random_number) = $image->out;

    $c->header_add(-type    => $mime_type);
    return $image_data;
}


1;

