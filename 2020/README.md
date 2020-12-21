# Notes on each day's solutions

![Completion times 2020](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2020.png)

(image by [Jo Wood](https://github.com/jwoLondon))

## [Day 1: Report Repair](https://adventofcode.com/2020/day/1)

Another simple one to start with.  The leaderboard times are misleading
because the servers were down for three and a half minutes.

Both parts were trivial with the right utility function from previous
years.  Some incorrect approaches would have failed on inputs including
1010, three numbers _n_, _n_, 2020-2*_n_ or two numbers _n_, 2020-2*_n_,
but my input had none of these.

## [Day 2: Password Philosophy](https://adventofcode.com/2020/day/2)

This was another easy list manipulation, after a little input processing.
I might have been tempted to write a more complex test for the second part,
if I hadn't already written an occurrence counter for the first part.
I messed up my first attempt by adding one rather than subtracting.

## [Day 3: Toboggan Trajectory](https://adventofcode.com/2020/day/3)

This was a fun little puzzle, described in terms of a repeating landscape,
but neatly solvable using arithmetic on coordinates.  Solutions using
`repeat` and indexing are shorter, and still feasible for the input size.

## [Day 4: Passport Processing](https://adventofcode.com/2020/day/4)

The second part involves checking some very detailed conditions.
Regular expressions would be very useful here, but its not too difficult
without them.  Some parts involve number ranges where both numbers
had the same number of digits, so string comparison would work, after
checking the length of the string (and it appears that the supplied
inputs included no cases where the lengths were wrong, such as "16cm").
I compared numbers, which seems easier to get right.

## [Day 5: Binary Boarding](https://adventofcode.com/2020/day/5)

A neat little puzzle after yesterday's nitpicking.  The first part is
binary representations and the second can be done cleanly with sets.

## [Day 6: Custom Customs](https://adventofcode.com/2020/day/6)

Both parts can be done cleanly and very concisely using bulk operations
on sets.

## [Day 7: Handy Haversacks](https://adventofcode.com/2020/day/7)

There was a bit of fussy parsing, but then some bulk operations on
containers.  The first part was more involved, inverting the relation
and then using breadth-first search.  For the second part, I memoized
the function because it is easy to do in a lazy language, but it turns
out that with this input it's not necessary.

## [Day 8: Handheld Halting](https://adventofcode.com/2020/day/8)

This is presumably the start of this year's virtual machine.  The first
part is a routine simulation, while the second is a novel piece of reverse
engineering.  Exhaustive search is sufficient.

## [Day 9: Encoding Error](https://adventofcode.com/2020/day/9)

The two parts are different list manipulations, and can be solved neatly
using a number of list library functions.  There is a corner case in the
first part (similar to day 1), but it is not tested by the supplied input.

My initial solution to the first part was ridiculously over-engineered,
but not noticeably faster than more straight-forward solutions.
This and the preceding two puzzles punish premature optimization.

## [Day 10: Adapter Array](https://adventofcode.com/2020/day/10)

The first part is straightforward once you get past the nonsensical
description, but the second is the most difficult so far.  As the text
suggests, exhaustive search is prohibitively expensive.

The sample inputs and the actual input all satisfy a crucial simplifying
condition that is not present in the text, though the first part is a
subtle hint.  Given that, we can decompose the problem, reducing it to
finding an integer sequence.  I worked out the first few values and then
found the sequence in [OEIS](https://oeis.org/), but it turns out that
the actual input only requires the first few values.

It seems most people on the [solutions thread](https://www.reddit.com/r/adventofcode/comments/ka8z8x/2020_day_10_solutions/)
used a dynamic programming approach over the whole list, which does not
rely on the above restriction in the input.

## [Day 11: Seating System](https://adventofcode.com/2020/day/11)

This is yet another variation on a cellular automaton.  Approaches similar
to those used on previous such puzzles get most of the way, but there are
a couple of novel twists.  It comes out quite neatly using bulk operations
on `Set`s and `Map`s, with most code shared between the two parts.

## [Day 12: Rain Risk](https://adventofcode.com/2020/day/12)

Both parts are following a sequence of instructions to move around
2-dimensional space, with the second only slightly more elaborate,
but easy enough after working out rotation.

## [Day 13: Shuttle Search](https://adventofcode.com/2020/day/13)

This is all modular arithmetic, with the twist that the remainders are
negated.  The second part becomes easy once it is recognized as the
[Chinese Remainder Problem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem),
previously seen on day 15 of 2016.  This is applicable if the moduli
are pairwise coprime, and in the given input they are all prime.
Plenty of test cases are supplied, so we can avoid wasted submissions.

## [Day 14: Docking Data](https://adventofcode.com/2020/day/14)

This exercise involved bit manipulation, with a novel twist in the
second part.  The calculations are precisely specified, but take some
care to implement.  In the given input, the nondeterminism is limited,
so a naive approach is effective.

The description specifies a fixed number of bits, but this could be
relaxed by assuming leading zeros.

## [Day 15: Rambunctious Recitation](https://adventofcode.com/2020/day/15)

This was novel.  It took a while to get part one working, partly getting
the switch between the given list and the generated outputs right,
but in the end it was fairly clean.  Since I'd already used a `Map`,
the same code should work for part two, but it had a fatal memory leak
due to lazy evaluation.  After searching unsuccessfully for a pattern
in the sequence, I fixed the memory leak with some strictness annotations.

A [reddit thread](https://www.reddit.com/r/adventofcode/comments/kdfvec/2020_day_15_theory_behind_the_problem/)
points out that this is a generalization of Van Eck's sequence
([OEIS A181391](https://oeis.org/A181391)), which has no known pattern.

## [Day 16: Report Repair](https://adventofcode.com/2020/day/16)

This was a nice exercise in decomposition and data manipulation.
The first part sets the stage, while the second part needs an efficient
algorithm to be feasible, in contrast to the brute-forceable early
questions.  The underlying problem is the same as in day 16 of 2018,
but I think I came up with a neater solution this time.

## [Day 17: Conway Cubes](https://adventofcode.com/2020/day/17)

Another cellular automaton, this time in 3 and 4 dimensions.  The examples
are worked out in detail, but it's simpler to use the specification,
which is clear enough by itself.  The different settings favour a generic
implementation, which was easily extracted from solutions to previous
automata puzzles.  It is a neat exercise in set-level operations.

I made a mistake with my 4-dimensional neighbours on the first attempt,
but as it was a separate function, it was easy to debug.

## [Day 18: Operation Order](https://adventofcode.com/2020/day/18)

This puzzle involves two variations on an expression parser.  After a
bit of transformation, the grammar can be implemented cleanly with a
recursive descent parser.  I had a simple library for that, but several
alternatives are available.

Instead of combining parsing and evaluation, I parsed to abstract syntax
and had a separate evaluation function.  That would have helped with
debugging the parser if anything had gone wrong, and I was able to re-use
the evaluator for the second part.

## [Day 19: Monster Messages](https://adventofcode.com/2020/day/19)

This puzzle involves recognizing strings generated by a context-free
grammar, first with a finite language, and then adjusted to make
it infinite.  The given grammar (like the examples) has several
simplifying properties, and part of the difficulty is deciding which
of them to use.  There are many possible solutions to choose between.

My initial recognizer for the first part was deterministic, and a
little analysis of the changes to the grammar suggested a way to use
it to solve the second part.  However, a nondeterministic recognizer
(no more complex than the deterministic one), would work without change
on the revised grammar of the second part.

## [Day 20: Jurassic Jigsaw](https://adventofcode.com/2020/day/20)

This puzzle was a considerable increase in difficulty, involving
assembling a grid of bitmaps so that the edges match, and then some
extra searching.  It can be done neatly by representing grids as lists
of lists, plus some use of of containers to speed up some parts.

The first part doesn't actually require a full assembly of the grid, just
the corners, but it seemed clear that the whole grid would be needed for
the second part.  My initial version attempted an optimized representation
of edges, but this turned out to be quite fiddly to get right, and
only gave a constant factor improvement.  It was both simpler and more
efficient to use maps to cut the branching in the backtracking search.

## [Day 21: Allergen Assessment](https://adventofcode.com/2020/day/21)

The problem description was very difficult to penetrate, partly because
the constraints make no sense in the presented scenario.  The example
is presented as an input-output pair with no working, because that would
give too much away.

The task itself is a moderately complex manipulation of sets.  I couldn't
see any shortcut to the first part without constructing the full matching,
so there wasn't much more to the second part.  From the Reddit thread, it
seems that simpler approaches worked with the supplied input.
