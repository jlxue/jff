package MarkIt::Captcha;
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
    my $q = $c->query;

    my $image = GD::SecurityImage->new(width    => 220,
                                       height   => 60,
                                       scramble => 1,
                                       lines    => 5 + int(rand(5)),
                                       thickness=> 1 + int(rand(1)),
                                       font     => '/usr/share/fonts/truetype/ttf-dejavu/DejaVuSansMono-Bold.ttf',
                                       ptsize   => 18,
                                       rnd_data => [A .. Z]);
    $image->random();
    $image->create("ttf", "ellipse");
    $image->particle(rand(2000) + 2000);

    my ($image_data, $mime_type, $random_str) = $image->out;

    $c->header_add(-expires         => "-1",
                   -Cache_Control   => "must-revalidate, no-cache, no-store",
                   -Pragma          => "no-cache",
                   -type            => "image/$mime_type");

    $c->session->param("captcha", $random_str);

    $c->log->debug("random str=$random_str\n");

    return $image_data;
}


1;

