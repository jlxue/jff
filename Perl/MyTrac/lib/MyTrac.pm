package MyTrac;

use warnings;
use strict;
use base 'Titanium';
use Carp;

our $VERSION = '0.01';

sub setup {
    my ($self) = @_;

    $self->start_mode('runmode1');
    $self->error_mode('runmode1');
    $self->run_modes( [qw/ runmode1 /] );
    if ( !$self->tmpl_path ) {
        ( my $tp = 'MyTrac' ) =~ s{-}{/}gmsx;
        $tp .= '/templates';
        foreach my $inc (@INC) {
            if ( -d "$inc/$tp" ) {
                $self->tmpl_path("$inc/$tp");
                last;
            }
        }
    }
    $self->run_modes( AUTOLOAD => 'runmode1' );
    return;
}

sub runmode1 {
    my ($self) = @_;

    my $template = $self->load_tmpl;
    $template->param( message => 'Hello world!' );
    return $template->output;
}


sub function1 {
    my ($self) = @_;

    return 1;
}

1;

__END__
