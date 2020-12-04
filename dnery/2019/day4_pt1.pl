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
my @indexes = (0..scalar @lines - 1);
my @epochs = map {
    $_ =~ /$date_pattern/;
    qx/date --date="$1" -u +%s/;
} @lines;
my @sorted_indexes = sort { $epochs[$a] <=> $epochs[$b] } @indexes;

my %minutes_asleep_by_guard_sum;  # stores sum of minutes asleep by guard
my %minutes_asleep_by_guard_map;  # stores full hash of minutes asleep by guard

# now populate the map of sleeping minute sums and hash of minutes asleep by guard
my $info_pattern = qr/^\[\d{4}-\d\d-\d\d \d\d:(\d\d)\] [^ ]+ (?|#(\d+)|(up)|(asleep)).*$/;
my $current_guard_id;
my $last_minute_asleep;

foreach (@sorted_indexes) {
    $lines[$_] =~ /$info_pattern/;
    my $info_message = $2;
    my $info_minute = $1;

    # if the message says "asleep", mark the timestamp and continue
    if ($info_message =~ /asleep/) {
        $last_minute_asleep = int $info_minute;
        next;
    }

    # if the message says "up", mark minutes and sum of minutes slept and continue
    if ($info_message =~ /up/) {
        $minutes_asleep_by_guard_sum{$current_guard_id} += int $info_minute - $last_minute_asleep;
        foreach my $idx($last_minute_asleep..(int $info_minute - 1)) {
            $minutes_asleep_by_guard_map{$current_guard_id}->{$idx}++;
        }
        next;
    }

    # otherwise, the message says the new guard on duty, so update the current guard id
    $current_guard_id = int $info_message;
}

# retrieve sleepiest guard from all guards' sleeping minute sums
my @sleepiest_guards = sort { $minutes_asleep_by_guard_sum{$a} <=>
    $minutes_asleep_by_guard_sum{$b} } keys %minutes_asleep_by_guard_sum;
my $sleepiest_guard = $sleepiest_guards[-1];

# retrieve sleepiest minute from sleepiest guard's sleeping minutes
my @sleepiest_minutes = sort { $minutes_asleep_by_guard_map{$sleepiest_guard}->{$a} <=>
    $minutes_asleep_by_guard_map{$sleepiest_guard}->{$b} } keys %{$minutes_asleep_by_guard_map{$sleepiest_guard}};
my $sleepiest_minute = $sleepiest_minutes[-1];

print $sleepiest_guard * $sleepiest_minute, "\n";
