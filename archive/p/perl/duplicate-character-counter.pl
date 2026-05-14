#!/usr/bin/env perl
use v5.42;

sub usage {
    say "Usage: please provide a string";
    exit 1;
}

my ($s) = @ARGV;

usage() unless defined $s;
usage() if $s eq '';

my %count;

for my $ch (split //, $s) {
    next unless $ch =~ /[A-Za-z0-9]/;
    $count{$ch}++;
}

my $found = false;

for my $ch (grep { $count{$_} > 1 } keys %count) {
    say "$ch: $count{$ch}";
    $found = true;
}

say "No duplicate characters" unless $found;