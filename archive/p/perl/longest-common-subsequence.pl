#!/usr/bin/env perl
use v5.42;

use List::Util qw(max);
use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say 'Usage: please provide two lists in the format "1, 2, 3, 4, 5"';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;

    my @vals = split /\s*,\s*/, $s;

    return undef if any { $_ !~ /\A\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

my ( $xs, $ys ) = @ARGV;

usage() unless defined $xs && defined $ys;

my $x = parse_list($xs) or usage();
my $y = parse_list($ys) or usage();

my $n = @$x;
my $m = @$y;

my @dp = map { [ (0) x ( $m + 1 ) ] } 0 .. $n;

for my $i ( 1 .. $n ) {
    for my $j ( 1 .. $m ) {
        if ( $x->[ $i - 1 ] == $y->[ $j - 1 ] ) {
            $dp[$i][$j] = $dp[ $i - 1 ][ $j - 1 ] + 1;
        }
        else {
            $dp[$i][$j] = max( $dp[ $i - 1 ][$j], $dp[$i][ $j - 1 ] );
        }
    }
}

my @out;
for ( my ( $i, $j ) = ( $n, $m ) ; $i && $j ; ) {
    if ( $x->[ $i - 1 ] == $y->[ $j - 1 ] ) {
        unshift @out, $x->[ $i - 1 ];
        $i--;
        $j--;
    }
    elsif ( $dp[ $i - 1 ][$j] >= $dp[$i][ $j - 1 ] ) {
        $i--;
    }
    else {
        $j--;
    }
}

say @out ? join( ", ", @out ) : "";
