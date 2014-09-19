#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;
use autodie;

use Text::Levenshtein qw(fastdistance);

my ( $f1, $f2 ) = @ARGV;

my $w1 = words($f1);
my $w2 = words($f2);

open my $gfh, '>', 'keeper.txt';
open my $bfh, '>', 'questionable.txt';

for my $w (@$w1) {
    my ( $d, $b ) = nearest( $w, $w2 );
    if ( $d <= 1 or $d == 2 && length($b) < length($w) && index( $w, $b ) > -1 )
    {
        print $bfh "$w $d $b\n";
    }
    else {
        print $gfh $w, "\n";
    }
}
close $gfh;
close $bfh;

sub nearest {
    my ( $word, $naughties ) = @_;
    my ( $min, $bad );
    for my $n (@$naughties) {
        my $d = fastdistance( $word, $n );
        if ($bad) {
            if ( $d < $min ) {
                $min = $d;
                $bad = $n;
            }
        }
        else {
            $min = $d;
            $bad = $n;
        }
    }
    return $min, $bad;
}

sub words {
    my $fn = shift;
    open my $fh, '<', $fn;
    my @words = <$fh>;
    close $fh;
    chomp @words;
    return \@words;
}
