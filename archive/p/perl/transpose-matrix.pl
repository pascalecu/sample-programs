#!/usr/bin/env perl
use v5.42;

use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say
"Usage: please enter the dimension of the matrix and the serialized matrix";
    exit;
}

sub parse_list ($s) {
    return undef unless defined $s;

    $s =~ s/\A\s+|\s+\z//g;
    return undef if $s eq '';

    my @vals = split /\s*,\s*/, $s;
    return undef if any { $_ !~ /\A-?\d+\z/ } @vals;

    return \@vals;
}

my ( $cols, $rows, $flat_s ) = @ARGV;

usage() unless defined $cols && defined $rows && defined $flat_s;
usage() if $cols eq '' || $rows eq '' || $flat_s eq '';
usage() unless $cols =~ /\A\d+\z/ && $rows =~ /\A\d+\z/;

my $flat = parse_list($flat_s) or usage();

my $n = @$flat;
usage() unless $n == $cols * $rows;

my @out;

for my $c ( 0 .. $cols - 1 ) {
    push @out, map $flat->[ $_ * $cols + $c ], 0 .. $rows - 1;
}

say join ", ", @out;
