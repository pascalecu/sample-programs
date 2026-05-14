#!/usr/bin/env perl
use v5.42;
use bigrat;

sub usage {
    say "Usage: ./fraction-math operand1 operator operand2";
    exit 1;
}

my ( $a_s, $op, $b_s ) = @ARGV;

usage() unless defined $a_s && defined $op && defined $b_s;
usage() if $a_s eq '' || $op eq '' || $b_s eq '';

my $a = (0 + $a_s) or usage();
my $b = (0 + $b_s) or usage();

my %ops = (
    '+' => sub { $a + $b },
    '-' => sub { $a - $b },
    '*' => sub { $a * $b },
    '/' => sub { $a / $b },

    '==' => sub { ( $a == $b ) ? 1 : 0 },
    '!=' => sub { ( $a != $b ) ? 1 : 0 },
    '>'  => sub { ( $a > $b )  ? 1 : 0 },
    '<'  => sub { ( $a < $b )  ? 1 : 0 },
    '>=' => sub { ( $a >= $b ) ? 1 : 0 },
    '<=' => sub { ( $a <= $b ) ? 1 : 0 },
);

usage() unless exists $ops{$op};

say $ops{$op}->();
