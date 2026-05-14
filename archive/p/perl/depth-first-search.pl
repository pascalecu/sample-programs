#!/usr/bin/env perl
use v5.42;

use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say 'Usage: please provide a tree in an adjacency matrix form ("0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0") together with a list of vertex values ("1, 3, 5, 2, 4") and the integer to find ("4")';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;
    return undef if $s =~ s/^\s+|\s+$//gr eq '';

    my @vals = split /\s*,\s*/, $s;
    return undef if any { $_ !~ /\A-?\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

sub build_matrix ($flat) {
    my $n = int sqrt(@$flat);
    return undef if $n * $n != @$flat;

    my @rows;
    while (@$flat) {
        push @rows, [ splice( @$flat, 0, $n ) ];
    }
    return \@rows;
}

sub dfs ( $adj, $values, $target ) {
    my $n = @$values;

    my @seen  = (0) x $n;
    my @stack = (0);

    while (@stack) {
        my $v = pop @stack;
        next if $seen[$v]++;

        return 1 if $values->[$v] == $target;

        my $row = $adj->[$v];

        for my $u ( 0 .. $#$row ) {
            push @stack, $u if $row->[$u];
        }
    }

    return 0;
}

my ( $tree_s, $vals_s, $target_s ) = @ARGV;

usage() unless @ARGV == 3;

my $tree_flat = parse_list($tree_s);
my $values    = parse_list($vals_s);

usage() unless $tree_flat && $values;

usage() unless $target_s =~ /\A-?\d+\z/;
my $target = 0 + $target_s;

my $adj = build_matrix($tree_flat) or usage();

usage() unless @$values == @$adj;

say dfs( $adj, $values, $target ) ? 'true' : 'false';
