#!/usr/bin/env perl
use v5.42;

sub usage {
    say
"Usage: please input the total number of people and number of people to skip.";
    exit 1;
}

sub is_int ($x) {
    return defined($x) && $x =~ /\A\d+\z/;
}

my ( $n, $k ) = @ARGV;

usage() unless is_int($n) && is_int($k);
usage() if $n eq '' || $k eq '';

$n = 0 + $n;
$k = 0 + $k;

usage() unless $n > 0 && $k > 0;

my $res = 0;

for my $i ( 2 .. $n ) {
    $res = ( $res + $k ) % $i;
}

say $res + 1;
