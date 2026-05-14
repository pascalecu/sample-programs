#!/usr/bin/env perl
use v5.42;

sub usage {
    say "Usage: please input a non-negative integer";
    exit;
}

my ($n) = @ARGV;

usage() unless defined $n;
$n =~ s/\A\s+|\s+\z//g;

usage() if $n eq '' || $n !~ /\A\d+\z/;

$n += 0;

my ( $a, $b ) = ( 1, 2 );
my @fib;

while ( $a <= $n ) {
    push @fib, $a;
    ( $a, $b ) = ( $b, $a + $b );
}

my @out;

for ( my $i = $#fib ; $i >= 0 ; ) {
    if ( $fib[$i] <= $n ) {
        push @out, $fib[$i];
        $n -= $fib[$i];
        $i -= 2;
    }
    else {
        $i--;
    }
}

say join ", ", @out;
