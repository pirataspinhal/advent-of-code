#!/usr/bin/env perl
use warnings;
use strict;

# open input
open my $fh, '<', $ARGV[0] or die $!;

my $all_twice = 0;      # count all exclusive x2 occurrences
my $all_thrice = 0;     # count all exclusive x3 occurrences

while (<$fh>) {
    chomp;              # clear leading and trailing spaces

    my $has_twice = 0;  # count local x2 occurrences
    my $has_thrice = 0; # count local x3 occurrences

    for my $char (split //) {
        if (m/^[^$char]*$char[^$char]*$char[^$char]*$char[^$char]*$/) {
            #printf "has %s thrice\n",$char;
            $has_thrice = 1;
        }
        if (m/^[^$char]*$char[^$char]*$char[^$char]*$/) {
            #printf "has %s twice\n",$char;
            $has_twice = 1;
        }
    }
    $all_twice += $has_twice;
    $all_thrice += $has_thrice;
}
close $fh;
print $all_twice * $all_thrice, "\n";
