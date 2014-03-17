#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;

my %words;
for my $file (<*>) {
    next if $file =~ /abbreviation|special/;
    say $file;
    open my $fh, '<', $file;
    for my $l (<$fh>) {
        next if $l =~ /[[:punct:]]/;
        $l =~ s/^\s+|\s+$//g;
        $words{ lc $l } = 1;
    }
}
say $_ for sort keys %words;
