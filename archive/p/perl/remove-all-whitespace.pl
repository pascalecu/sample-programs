#!/usr/bin/env perl
use v5.42;

sub usage {
    say 'Usage: please provide a string';
    exit;
}

my ($s) = @ARGV;

usage() unless defined $s;
usage() if $s eq '';

$s =~ s/\s+//g;
say $s;
