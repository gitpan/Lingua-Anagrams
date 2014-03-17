#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;

use Lingua::Anagrams;

open my $fh, '<', 'wordsEn.txt' or die "Aargh! $!";
my @words = <$fh>;
close $fh;

my $anagramizer = Lingua::Anagrams->new( \@words, limit => 30);

my $t1       = time;
my @anagrams = $anagramizer->anagrams('david fairchild houghton');
my $t2       = time;

say join ' ', @$_ for @anagrams;
say "";
say scalar(@anagrams) . ' anagrams';
say 'it took ' . ( $t2 - $t1 ) . ' seconds';
