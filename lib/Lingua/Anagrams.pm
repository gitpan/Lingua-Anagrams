package Lingua::Anagrams;
$Lingua::Anagrams::VERSION = '0.008';
# ABSTRACT: pure Perl anagram finder

use strict;
use warnings;


# don't cache anagrams for bigger character counts than this
our $LIMIT = 20;

# some global variables to be localized
# used to limit time spent copying values
our ( $limit, $known, $trie, %cache, $cleaner, @jumps, %word_cache, @indices );


sub new {
    my ( $class, $words, %params ) = @_;
    $class = ref $class || $class;
    local $cleaner = $params{clean} // \&_clean;
    my ( $trie, $known, $lowest ) = _trieify($words);
    die 'no words' unless $lowest;
    return bless {
        limit => $params{limit} // $LIMIT,
        clean => $cleaner,
        trie  => $trie,
        known => $known,
    }, $class;
}

sub _trieify {
    my $words = shift;
    my $base  = [];
    my ( @known, $lowest );
    my $terminal = [];
    for my $word (@$words) {
        $cleaner->($word);
        next unless length $word;
        my @chars = map ord, split //, $word;
        for my $i (@chars) {
            if ( defined $lowest ) {
                $lowest = $i if $i < $lowest;
            }
            else {
                $lowest = $i;
            }
        }
        _learn( \@known, \@chars );
        push @chars, 0;
        _add( $base, \@chars, $terminal );
    }
    return $base, \@known, $lowest;
}

sub _learn {
    my ( $known, $new ) = @_;
    for my $i (@$new) {
        $known->[$i] ||= 1;
    }
}

sub _add {
    my ( $base, $chars, $terminal ) = @_;
    my $i = shift @$chars;
    if ($i) {
        my $next = $base->[$i] //= [];
        _add( $next, $chars, $terminal );
    }
    else {
        $base->[0] //= $terminal;
    }
}

# walk the trie looking for words you can make out of the current character count
sub _words_in {
    my ( $counts, $total ) = @_;
    my @words;
    my @stack = ( [ 0, $trie ] );
    while (1) {
        my ( $c, $level ) = @{ $stack[-1] };
        if ( $c == -1 || $c >= @$level ) {
            last if @stack == 1;
            pop @stack;
            ++$total;
            $c = \( $stack[-1][0] );
            ++$counts->[$$c];
            $$c = $jumps[$$c];
        }
        else {
            my $l = $level->[$c];
            if ($l) {    # trie holds corresponding node
                if ($c) {    # character
                    if ( $counts->[$c] ) {
                        push @stack, [ 0, $l ];
                        --$counts->[$c];
                        --$total;
                    }
                    else {
                        $stack[-1][0] = $jumps[$c];
                    }
                }
                else {       # terminal
                    my $w = join '',
                      map { chr( $_->[0] ) } @stack[ 0 .. $#stack - 1 ];
                    $w = $word_cache{$w} //= scalar keys %word_cache;
                    push @words, [ $w, [@$counts] ];
                    if ($total) {
                        $stack[-1][0] = $jumps[$c];
                    }
                    else {
                        pop @stack;
                        ++$total;
                        $c = \( $stack[-1][0] );
                        ++$counts->[$$c];
                        $$c = $jumps[$$c];
                    }
                }
            }
            else {
                $stack[-1][0] = $jumps[$c];
            }
        }
    }
    \@words;
}


sub anagrams {
    my ( $self, $phrase ) = @_;
    local ( $trie, $known, $limit, $cleaner ) =
      @$self{qw(trie known limit clean)};
    $cleaner->($phrase);
    return () unless length $phrase;
    my $counts = _counts($phrase);
    return () unless _all_known($counts);
    local @jumps      = _jumps($counts);
    local @indices    = _indices($counts);
    local %cache      = ();
    local %word_cache = ();
    my @anagrams = _anagramize($counts);
    return () unless @anagrams;
    my %r = reverse %word_cache;
    @anagrams = map {
        [ sort map { $r{$_} } @$_ ]
    } @anagrams;
    return @anagrams;
}

sub _indices {
    my $counts = shift;
    my @indices;
    for my $i ( 0 .. $#$counts ) {
        push @indices, $i if $counts->[$i];
    }
    return @indices;
}

sub _jumps {
    my $counts = shift;
    my @jumps  = (0) x @$counts;
    my $j      = 0;
    while ( my $n = _next_jump( $counts, $j ) ) {
        $jumps[$j] = $n;
        $j = $n;
    }
    $jumps[-1] = -1;
    return @jumps;
}

sub _next_jump {
    my ( $counts, $j ) = @_;
    for my $i ( $j + 1 .. $#$counts ) {
        return $i if $counts->[$i];
    }
    return;
}

sub _clean {
    $_[0] =~ s/\W+//g;
    $_[0] = lc $_[0];
}

sub _all_known {
    my $counts = shift;
    return if @$counts > @$known;
    for my $i ( 0 .. $#$counts ) {
        return if $counts->[$i] && !$known->[$i];
    }
    return 1;
}

sub _counts {
    my $phrase = shift;
    $phrase =~ s/\s//g;
    my @counts;
    for my $c ( map ord, split //, $phrase ) {
        $counts[$c]++;
    }
    $_ //= 0 for @counts;
    return \@counts;
}

sub _any {
    for my $v ( @{ $_[0] } ) {
        return 1 if $v;
    }
    '';
}

sub _anagramize {
    my $counts = shift;
    my $total  = 0;
    $total += $_ for @$counts[@indices];
    my $key;
    if ( $total <= $limit ) {
        $key = join ',', @$counts[@indices];
        my $cached = $cache{$key};
        return @$cached if $cached;
    }
    my @anagrams;
    my $words = _words_in( $counts, $total );
    if ( _all_touched( $counts, $words ) ) {
        for (@$words) {
            my ( $word, $c ) = @$_;
            if ( _any($c) ) {
                push @anagrams, [ $word, @$_ ] for _anagramize($c);
            }
            else {
                push @anagrams, [$word];
            }
        }
        my %seen;
        @anagrams = map {
            $seen{ join ' ', sort { $a <=> $b } @$_ }++
              ? ()
              : $_
        } @anagrams;
    }
    $cache{$key} = \@anagrams if $key;
    @anagrams;
}

sub _all_touched {
    my ( $counts, $words ) = @_;

    my ( $first_index, $c );

    # if any letter count didn't change, there's no hope
  OUTER: for my $i (@indices) {
        next unless $c = $counts->[$i];
        $first_index //= $i;
        for (@$words) {
            next OUTER if $_->[1][$i] < $c;
        }
        return;
    }

    # we only need consider all the branches which affected a
    # particular letter; we will find all possibilities in their
    # ramifications
    $c = $counts->[$first_index];
    @$words = grep { $_->[1][$first_index] < $c } @$words;
    return 1;
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Lingua::Anagrams - pure Perl anagram finder

=head1 VERSION

version 0.008

=head1 SYNOPSIS

  use Lingua::Anagrams;

  open my $fh, '<', 'wordsEn.txt' or die "Aargh! $!";
  my @words = <$fh>;
  close $fh;

  my $anagramizer = Lingua::Anagrams->new( \@words );  # NOT a good word list for this purpose

  my $t1       = time;
  my @anagrams = $anagramizer->anagrams('Find anagrams!');
  my $t2       = time;

  print join ' ', @$_ for @anagrams;
  print "\n\n";
  print scalar(@anagrams) , " anagrams\n"";
  print 'it took ' , ( $t2 - $t1 ) , " seconds\n"";

Giving you

  ...
  naif nm rag sad
  naif nm raga sd
  naif nm rd saga
  naif ragman sd

  20906 anagrams
  it took 3 seconds

=head1 DESCRIPTION

L<Lingua::Anagrams> constructs a trie out of a list of words you give it. It then uses this
trie to find all the anagrams of a phrase you give to its C<anagrams> method. A dynamic
programming algorithm is used to accelerate at the cost of memory. See C<new> for how one may
modify this algorithm.

Be aware that the anagram algorithm has been golfed down pretty far to squeeze more speed out
of it. It isn't the prettiest.

=head1 METHODS

=head2 CLASS->new( $word_list, %params )

Construct a new anagram engine from a word list. The parameters understood
by the constructor are

=over 4

=item limit

The character count limit used by the dynamic programming algorithm to throttle memory
consumption somewhat. If you wish to find the anagrams of a very long phrase you may
find the caching in the dynamic programming algorithm consumes too much memory. Set this
limit lower to protect yourself from memory exhaustion (and slow things down).

The default limit is set by the global C<$LIMIT> variable. It will be 20 unless you
tinker with it.

=item clean

A code reference specifying how text is to be cleaned of extraneous characters
and normalized. The default cleaning function is

  sub _clean {
      $_[0] =~ s/\W+//g;
      $_[0] = lc $_[0];
  }

Note that this function, like C<_clean>, must modify its argument directly.

=back

=head2 $self->anagrams( $phrase )

Returns a list of array references, each reference containing a list of
words which together constitute an anagram of the phrase.

=head1 SOME CLEVER BITS

One trick I use to speed things up is to convert all characters to integers
immediately. If you're using integers, you can treat arrays are really fast
hashes.

Another is, in the trie, to build the trie only of arrays. The character
identity is encoded in the array position, the distance from the start by depth.
So the trie contains nothing but arrays. For a little memory efficiency the
terminal symbols is always the same empty array.

The natural way to walk the trie is with recursion, but I use a stack and a loop
to speed things up.

I use a jump table to keep track of the actual characters under consideration so
when walking the trie I only consider characters that might be in anagrams.

A particular step of anagram generation consists of pulling out all words that
can be formed with the current character counts. If a particular character count
is not decremented in the formation of any word in a given step we know we've
reached a dead end and we should give up.

Similarly, if we B<do> touch every character in a particular step we can collect
all the words extracted which touch that character and descend only into the
remaining possibilities for those character counts because the other words one
might exctract are necessarily contained in the remaining character counts.

The dynamic programming bit consists of memoizing the anagram lists keyed to the
character counts so we never extract the anagrams for a particular set of counts
twice (of course, we have to calculate this key many times, which is not free).

I localize a bunch of variables on the first method call so that thereafter
these values can be treated as global. This saves a lot of copying.

After the initial method calls I use functions, which saves a lot of lookup time.

In stack operations I use push and pop in lieu of unshift and shift. The former
are more efficient, especially with short arrays.

=head1 AUTHOR

David F. Houghton <dfhoughton@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2014 by David F. Houghton.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
