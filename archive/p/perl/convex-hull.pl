#!/usr/bin/env perl
use v5.42;

use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say 'Usage: please provide at least 3 x and y coordinates as separate lists (e.g. "100, 440, 210")';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;

    my @vals = split /\s*,\s*/, $s;

    return undef if @vals < 3;
    return undef if any { $_ !~ /\A-?\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

sub cross ( $o, $a, $b ) {
    ( $a->[0] - $o->[0] ) * ( $b->[1] - $o->[1] ) -
      ( $a->[1] - $o->[1] ) * ( $b->[0] - $o->[0] );
}

sub build_half ( $points, $reverse = false ) {
    my @hull;

    for my $p ( $reverse ? reverse @$points : @$points ) {
        pop @hull
          while ( @hull >= 2 && cross( $hull[-2], $hull[-1], $p ) <= 0 );
        push @hull, $p;
    }

    pop @hull;
    return @hull;
}

sub convex_hull ($points) {
    my @pts = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @$points;

    my @lower = build_half( \@pts );
    my @upper = build_half( \@pts, true );

    return [ @lower, @upper ];
}

my ( $xs, $ys ) = @ARGV;

my $x = parse_list($xs);
my $y = parse_list($ys);

usage() unless $x && $y && @$x == @$y;
usage() if @$x < 3;

my @points = map { [ $x->[$_], $y->[$_] ] } 0 .. $#$x;

my $hull = convex_hull( \@points );

say "($_->[0], $_->[1])" for @$hull;
