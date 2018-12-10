#/usr/bin/env perl
use diagnostics;
use warnings;
use strict;

# read all input
open my $fh, '<', $ARGV[0] or die $!;
chomp(my @lines = <$fh>);
close $fh;

# generate a list of sorted indexes
my $date_pattern = qr/^\[([^\[]+)\].+$/;
my @epochs = map {
    $_ =~ /$date_pattern/;
    qx/date --date="$1" -u +%s/;
} @lines;

my @indexes = (0..scalar @lines - 1);
my @sorted_indexes = sort { $epochs[$a] <=> $epochs[$b] } @indexes;

foreach (@sorted_indexes) {
    print $lines[$_], "\n";
}
