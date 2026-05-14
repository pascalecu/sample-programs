#!/usr/bin/env perl
use v5.42;

use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say "Usage: please provide a list of profits and a list of deadlines";
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;
    return undef if $s =~ s/^\s+|\s+$//gr eq '';

    my @vals = split /\s*,\s*/, $s;
    return undef if any { $_ !~ /\A\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

my ( $p_s, $d_s ) = @ARGV;

usage() unless defined $p_s && defined $d_s;
usage() if $p_s eq '' || $d_s eq '';

my $profits   = parse_list($p_s) or usage();
my $deadlines = parse_list($d_s) or usage();

usage() unless @$profits == @$deadlines;

my @jobs;
my $max_d = 0;

for my $i ( 0 .. $#$profits ) {
    my $p = $profits->[$i];
    my $d = $deadlines->[$i];

    push @jobs, [ $p, $d ];
    $max_d = $d if $d > $max_d;
}

my @parent = ( 0 .. $max_d );

sub find ($x) {
    return 0 if $x <= 0;

    while ( $parent[$x] != $x ) {
        $parent[$x] = $parent[ $parent[$x] ];
        $x = $parent[$x];
    }

    return $x;
}

sub occupy ($slot) {
    $parent[$slot] = find( $slot - 1 );
}

@jobs = sort { $b->[0] <=> $a->[0] } @jobs;

my $total = 0;

for my $job (@jobs) {
    my ( $profit, $deadline ) = @$job;

    my $slot = find($deadline);
    next unless $slot;

    $total += $profit;
    occupy($slot);
}

say $total;
