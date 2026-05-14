#!/usr/bin/env perl
use v5.42;
use List::Util qw/max sum0/;
use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say
      'Usage: Please provide a list of integers in the format: "1, 2, 3, 4, 5"';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;
    return undef if $s =~ s/^\s+|\s+$//gr eq '';

    my @vals = split /\s*,\s*/, $s;
    return undef if any { $_ !~ /\A-?\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

my ($s) = @ARGV;
usage() unless defined $s;

my $a = parse_list($s) or usage();

my $has_negative = any { $_ < 0 } @$a;
unless ($has_negative) {
    say sum0 @$a;
    exit 0;
}

my ( $cur, $best ) = ( $a->[0] ) x 2;

for my $x ( @$a[ 1 .. $#$a ] ) {
    $cur  = max( $x,    $cur + $x );
    $best = max( $best, $cur );
}

say $best;
