
=head1 NAME

MyTrac - The great new MyTrac web application!

=head1 SYNOPSIS

    use MyTrac;
    my $app = MyTrac->new();
    $app->run();

=head1 ABSTRACT

A brief summary of what MyTrac does.

=cut

package MyTrac;

use warnings;
use strict;
use base 'Titanium';
use Carp qw( croak );

=head1 VERSION

This document describes MyTrac Version 0.01

=cut

our $VERSION = '0.01';

=head1 DESCRIPTION

Overview of functionality and purpose of
web application module MyTrac...

=head1 METHODS

=head2 SUBCLASSED METHODS

=head3 setup

Sets up the run mode dispatch table and the start, error, and default run modes.
If the template path is not set, sets it to a default value.

TODO: change all these values to ones more appropriate for your application.

=cut

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

=pod

TODO: Other methods inherited from Titanium go here.

=head2 RUN MODES

=head3 runmode1

  * Purpose
  * Expected parameters
  * Function on success
  * Function on failure

TODO: Describe runmode1 here. Rename runmode1 to something more appropriate 
for your application.

=cut

sub runmode1 {
    my ($self) = @_;

    my $template = $self->load_tmpl;
    $template->param( message => 'Hello world!' );
    return $template->output;
}

=head2 OTHER METHODS

=head3 function1

TODO: Describe function1 here.  Rename function1 to something more appropriate
for your application.

=cut

sub function1 {
    my ($self) = @_;

    return 1;
}

=pod

TODO: Other methods in your public interface go here.

=cut

# TODO: Private methods go here. Start their names with an _ so they are skipped
# by Pod::Coverage.

=head1 BUGS AND LIMITATIONS

There are no known problems with this module.

Please report any bugs or feature requests to
C<bug-mytrac at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=MyTrac>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SEE ALSO

L<Titanium>

=head1 THANKS

List acknowledgements here or delete this section.

=head1 AUTHOR

Liu Yubao, C<< <Yubao.Liu at gmail.com> >>

=head1 LICENSE AND COPYRIGHT

Copyright 2010 Liu Yubao, all rights reserved.

This distribution is free software; you can redistribute it and/or modify it
under the terms of either:

a) the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version, or

b) the Artistic License version 2.0.

The full text of the license can be found in the LICENSE file included
with this distribution.

=cut

1;    # End of MyTrac

__END__
