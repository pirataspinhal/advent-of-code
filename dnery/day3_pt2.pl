#/usr/bin/env perl
use warnings;
use strict;

# open input
open my $fh, '<', $ARGV[0] or die $!;

my $claim = qr/^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$/; # used to extract data from claim
my %occupation_map;                                                 # tracks occupied squares (w/ claim number)
my %intact_map;                                                     # tracks intact squares

while (<$fh>) {
    $_ =~ /$claim/;
    my $claim_no = $1;
    $intact_map{$claim_no} = 1;

    for (my $i = $2; $i < $2 + $4; $i++) {
        for (my $j = $3; $j < $3 + $5; $j++) {
            my $index = $i + 1000 * $j;

            # for every square of this claim, if collision happens, mark both as not intact
            if (my $other_claim_no = $occupation_map{$index}) {
                $intact_map{$other_claim_no} = 0;
                $intact_map{$claim_no} = 0;
            }

            # ovewrite current occupation map with latest claim
            $occupation_map{$index} = $claim_no;
        }
    }
}
close $fh;

# now find the only intact claim
foreach (keys %intact_map) {
    if ($intact_map{$_}) {
        print $_, "\n";
        last;
    }
}
