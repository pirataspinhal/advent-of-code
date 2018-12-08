#!/usr/bin/env perl
use warnings;
use strict;

# read all input
open my $fh, '<', $ARGV[0] or die $!;
chomp(my @lines = <$fh>);
close $fh;

outer_most: {
    # iterate trough every character
    for (my $i = 0; $i < length $lines[0]; $i++) {
        my %str_map;

        # iterate through every line
        foreach my $line (@lines) { # THIS IS A REFERENCE
            my $line_copy = $line;  # THIS IS A DEEP COPY

            #chomp this character
            substr $line_copy, $i, 1, '';

            if ($str_map{$line_copy}) {
                print $line_copy, "\n";
                last outer_most;
            }
            $str_map{$line_copy} = 1;
        }
        undef %str_map;
    }
}
