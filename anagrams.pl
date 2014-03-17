#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;

use Lingua::Anagrams;

say 'reading word list...';
open my $fh, '<', 'words.txt' or die "Aargh! $!";
my @words = <$fh>;
close $fh;

say 'processing word list...';
my $anagramizer = Lingua::Anagrams->new( \@words );

say 'starting...';
my $t1       = time;
my @anagrams = $anagramizer->anagrams('pig farmer o');
my $t2       = time;

say join ' ', @$_ for @anagrams;
say "";
say scalar(@anagrams) . ' anagrams';
say 'it took ' . ( $t2 - $t1 ) . ' seconds';
