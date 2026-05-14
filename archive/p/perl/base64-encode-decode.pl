#!/usr/bin/env perl
use v5.42;
use MIME::Base64 qw(encode_base64 decode_base64);

sub usage {
    say "Usage: please provide a mode and a string to encode/decode";
    exit 1;
}

my ( $mode, $text ) = @ARGV;

usage() if !defined $mode || !defined $text;
usage() if $mode eq ''    || $text eq '';

my $BASE64_RE = qr{
    ^
    # Any number of full 4-char byte chunks
    (?:[A-Za-z0-9+/]{4})*

    (?:                                      # either
        [A-Za-z0-9+/]{2}[AEIMQUYcgkosw048]=  # one pad char
      |                                      # or
        [A-Za-z0-9+/][AQgw]==                # two pad chars
    )?
    \z
}x;

sub valid_decode ($t) {
    return 0 if !defined $t || $t eq '';
    return $t =~ $BASE64_RE;
}

if ( $mode eq 'encode' ) {
    say encode_base64( $text, '' );
    exit 0;
}

if ( $mode eq 'decode' ) {
    usage() unless valid_decode($text);
    say decode_base64($text);
    exit 0;
}

usage();
