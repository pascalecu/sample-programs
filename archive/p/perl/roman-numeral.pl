#!/usr/bin/env perl
use v5.42;

my %VAL = (
    I => 1,
    V => 5,
    X => 10,
    L => 50,
    C => 100,
    D => 500,
    M => 1000,
);

sub usage   { say "Usage: please provide a string of roman numerals"; exit }
sub invalid { say "Error: invalid string of roman numerals";          exit }

my ($s) = @ARGV;

defined $s or usage();

$s =~ s/\A\s+|\s+\z//g;
$s eq '' and say 0 and exit;

$s =~ /\A[IVXLCDM]+\z/ or invalid();

my ( $total, $prev ) = ( 0, 0 );

for my $c ( reverse split //, $s ) {
    my $v = $VAL{$c};
    $total += $v < $prev ? -$v : $v;
    $prev = $v;
}

say $total;
