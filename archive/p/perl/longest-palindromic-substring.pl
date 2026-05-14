#!/usr/bin/env perl
use v5.42;
use List::Util qw(max);

sub usage {
    say "Usage: please provide a string that contains at least one palindrome";
    exit 1;
}

my ($s) = @ARGV;

usage() unless defined $s;
$s =~ s/^\s+|\s+$//g;
usage() if $s eq '';

my @chars = split //, $s;
my $t     = '^#' . join( '#', @chars ) . '#$';

my $n = length($t);
my @p = (0) x $n;

my ( $center, $right ) = ( 0, 0 );

for my $i ( 1 .. $n - 2 ) {

    my $mirror = 2 * $center - $i;

    if ( $i < $right ) {
        $p[$i] = max( 0, $right - $i, $p[$mirror] );
    }

    my $left    = $i - 1 - $p[$i];
    my $right_i = $i + 1 + $p[$i];

    while ( substr( $t, $left, 1 ) eq substr( $t, $right_i, 1 ) ) {
        $p[$i]++;
        $left--;
        $right_i++;
    }

    if ( $i + $p[$i] > $right ) {
        $center = $i;
        $right  = $i + $p[$i];
    }
}

my ( $len, $idx ) = ( 0, 0 );

for my $i ( 0 .. $#p ) {
    ( $len, $idx ) = ( $p[$i], $i ) if $p[$i] > $len;
}

my $start  = ( $idx - $len ) >> 1;
my $result = substr( $s, $start, $len );

usage() if length($result) < 2;

say $result;
