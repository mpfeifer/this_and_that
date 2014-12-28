#!/usr/bin/perl

use strict;
use IO::Socket::INET;
use Log::Log4perl qw(:easy);
use Getopt::Long;

my $easybox = "";

GetOptions("easybox=s" => \$easybox);

Log::Log4perl->easy_init($ERROR);

my $logger = get_logger();

$logger->debug("Script wanip.pl starting up.");

# flush after every write
$| = 1;

my $socket;

# creating object interface of IO::Socket::INET modules which internally creates 
# socket, binds and connects to the TCP server running on the specific port.

$socket = new IO::Socket::INET (
PeerHost => "$easybox",
PeerPort => '80',
Proto => 'tcp',
) or die "ERROR in Socket Creation : $!\n";

$logger->debug("TCP Connection Success.");

my $http_request = "GET / HTTP/1.1\r\nHost: 192.168.2.1\r\n\r\n";

$logger->debug("Sending HTTP Get request: $http_request");

$socket->send( $http_request );
my $data = "";

# read the socket data sent by server.
while (<$socket>) {
  $logger->debug("Received data from PeerHost: $data");
  $data .= $_;
}

$data =~ m/STR_WAN_IP_address....([0-9.]+)/;

$logger->debug("Found wan ip address: $1");

print "$1";

$socket->close();

$logger->debug("Script terminated.");
