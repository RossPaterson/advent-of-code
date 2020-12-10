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
