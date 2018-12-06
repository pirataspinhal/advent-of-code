#!/usr/bin/env perl
use warnings;
use strict;

# open input
open my $fh, $ARGV[0] or die $!;

# do sum
my $total=0;
while (<$fh>) {
    $total += $_;
}

printf "%d\n",$total;
