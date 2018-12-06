#!/usr/bin/env perl
use warnings;
use strict;

# open input
open my $fh, $ARGV[0] or die $!;

# engage map
my %possibles;
my $current=0;
$possibles{$current}=1;

# do repeated sum
my $looping=1;
while ($looping) {
    while (<$fh>) {
        $current += $_;
        if ($possibles{$current}) {
            printf "Found!\n";
            $looping=0;
            last;
        }
        $possibles{$current}=1;
    }
    seek $fh, 0, 0;
}

printf "%d\n",$current;
