#!/usr/bin/env perl
use v5.42;
use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say 'Usage: please provide a comma-separated list of integers';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;

    $s =~ s/^\s+|\s+$//g;
    return undef if $s eq '';

    my @vals = split /\s*,\s*/, $s;

    return undef if any { $_ !~ /\A\d+\z/ } @vals;

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

sub mst_cost ($g) {
    my $n = @$g;

    my @key  = ( ( 0 + 'inf' ) x $n );
    my @used = (0) x $n;

    $key[0] = 0;
    my $total = 0;

    for ( 1 .. $n ) {
        my ($u) =
          grep { !$used[$_] }
          sort { $key[$a] <=> $key[$b] } 0 .. $n - 1;

        return undef unless defined $u;

        $used[$u] = 1;
        $total += $key[$u];

        for my $v ( 0 .. $n - 1 ) {
            my $w = $g->[$u][$v];
            next if !$w || $used[$v];

            $key[$v] = $w if $w < $key[$v];
        }
    }

    return $total;
}

my ($s) = @ARGV;
usage() unless defined $s;

my $flat = parse_list($s) or usage();
my $g    = build_matrix($flat) or usage();

say mst_cost($g) // usage();