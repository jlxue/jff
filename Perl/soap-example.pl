#!/usr/bin/perl -w
use strict;
use warnings;
use SOAP::Lite;
use Data::Dumper;

{
    # https://api.yieldmanager.com/doc/9_issues.html
    #
    # You need to put the following code to deserialize value of enum type
    # correctly, it is caused by SOAP::Lite's inability to parse responses
    # which return enum types. 
    no warnings 'redefine';
    sub SOAP::Deserializer::typecast {
        my ($self, $value, $name, $attrs, $children, $type) = @_;
        return undef unless defined $type;
        return ( $type =~ /enum/ ) ? $value : undef;
    }
}

my $pixel_client = SOAP::Lite->new (
    proxy => "https://xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/latest/pixel.php",
    ns    => "urn:PixelService",
);

my $token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";

if( !defined($pixel_client)) {
    print STDERR "Can't create pixel client: $!\n";
    exit -1;
}

# getAll() returns a SOAP::SOM object
my ($pixels, $total_count) = $pixel_client->getAll($token, 2, 1)->paramsall;
print Dumper($pixels), "$total_count\n";

