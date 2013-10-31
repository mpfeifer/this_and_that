#!/usr/bin/perl

use Getopt::Long;
use File::Find;
use File::Copy;
use strict;

my $counter=0;
my $target="/cygdrive/h/temp/";
my @source = ();
my $ext = "xyz";

# TODO
# -t target directory
# -s source directory
# do some logging (via log4perl...)
# do include some debugging information
# do source with comma seperated value

GetOptions("target=s" => \$target,
           "source=s" => \@source );

print "Copying files from @source to $target\n";

# initialize counter variable to maximum value found plus one
 sub findCounter {
    if (m/^(0*)([1-9][0-9]*)\./) {
        if ($2 > $counter) {
            $counter=$2;
        }
    }
}
find(\&findCounter, ($target));
$counter=$counter+1;
print "counter initialized to $counter\n";

# This routine defines how to copy files from source to target
sub copyImages {
    $ext = "xyz";
    if ( -f $File::Find::name) {
        if ( m/.*(JPG|jpg|jpeg)$/) {
            $ext = "jpg";
        } elsif ( m/.*(png|PNG)$/) {
            $ext = "png";
        }
    }
     if ( $ext !~ /xyz/ )  {
        my $filename = sprintf("%08d.%s", $counter, $ext);
        print $File::Find::name . " -> " . $filename . "\n";
        copy($File::Find::name, $target . $filename );
        $counter = $counter + 1;
    }
}
# Now start copying files from source to target
find(\&copyImages, @source);
