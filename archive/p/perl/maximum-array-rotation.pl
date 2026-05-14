#!/usr/bin/env perl
use v5.42;
use feature 'keyword_any';
no warnings 'experimental::keyword_any';

sub usage {
    say 'Usage: please provide a list of integers (e.g. "8, 3, 1, 2")';
    exit 1;
}

sub parse_list ($s) {
    return undef unless defined $s;
    return undef if $s =~ s/^\s+|\s+$//gr eq '';

    my @vals = split /\s*,\s*/, $s;
    return undef if any { $_ !~ /\A\d+\z/ } @vals;

    return [ map 0 + $_, @vals ];
}

my ($s) = @ARGV;
usage() unless defined $s;

my $a = parse_list($s) or usage();
my $n = @$a;

my $sum = 0;
my $cur = 0;

for my $i ( 0 .. $n - 1 ) {
    $sum += $a->[$i];
    $cur += $i * $a->[$i];
}

my $best = $cur;

for my $i ( 1 .. $n - 1 ) {
    $cur += $sum - $n * $a->[ $n - $i ];
    $best = $cur if $cur > $best;
}

say $best;
