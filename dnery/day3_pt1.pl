#/usr/bin/env perl
use warnings;
use strict;

# open input
open my $fh, '<', $ARGV[0] or die $!;

my $claim = qr/^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$/; # used to extract data from claim
my $collision_count = 0;                                            # counts unique collisions
my %occupation_map;                                                 # tracks occupied squares

while (<$fh>) {
    $_ =~ /$claim/;

    for (my $i = $2; $i < $2 + $4; $i++) {
        for (my $j = $3; $j < $3 + $5; $j++) {
            my $index = $i + 1000 * $j;

            # for every square of this claim, count collisions (if first) and add to occupation map
            if ($occupation_map{$index} && $occupation_map{$index} == 1) {
                $collision_count++;
            }
            $occupation_map{$index}++;
        }
    }
}
close $fh;
print $collision_count, "\n";
