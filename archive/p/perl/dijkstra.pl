#!/usr/bin/env perl
use v5.42;
use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say
'Usage: please provide three inputs: a serialized matrix, a source node and a destination node';
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

sub dijkstra ( $g, $src, $dst ) {
    my $n = @$g;

    my $INF = 0 + 'inf';

    my @dist = ($INF) x $n;
    my @seen = (0) x $n;

    $dist[$src] = 0;

    for ( 1 .. $n ) {

        my $u = -1;

        for my $i ( 0 .. $n - 1 ) {
            next if $seen[$i];

            if ( $u == -1 || $dist[$i] < $dist[$u] ) {
                $u = $i;
            }
        }

        last if $u == -1 || $dist[$u] == $INF;

        $seen[$u] = 1;

        for my $v ( 0 .. $n - 1 ) {
            my $w = $g->[$u][$v];

            next if $w <= 0;

            my $nd = $dist[$u] + $w;

            $dist[$v] = $nd if $nd < $dist[$v];
        }
    }

    return undef if $dist[$dst] == $INF;
    return $dist[$dst];
}

my ( $matrix_s, $src_s, $dst_s ) = @ARGV;

usage() unless @ARGV == 3;

my $flat = parse_list($matrix_s);
usage() unless $flat;

my $adj = build_matrix($flat) or usage();

my $n = @$adj;

usage() unless $src_s =~ /\A\d+\z/;
usage() unless $dst_s =~ /\A\d+\z/;

my $src = 0 + $src_s;
my $dst = 0 + $dst_s;

usage() if $src >= $n || $dst >= $n;

my $cost = dijkstra($adj, $src, $dst);
usage() unless defined $cost;

say $cost;
