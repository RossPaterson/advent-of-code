# Notes on each day's solutions

![Completion times 2022](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2022.png)

(image by [Jo Wood](https://github.com/jwoLondon))

## [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

This was an easy exercise with list functions, directly implementing
the description.

## [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

This involves simple functions on values.  It shouldn't have been
difficult, but I got in a tangle mixing up the four 3-value types that
were involved.

## [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

An easy exercise using lists and sets.

## [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)

This one involves a little parsing, and then basic range operations,
with a neat symmetry between the parts.

## [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)

After messy input parsing, the main computation is list processing,
which works out neatly using list functions.  The second part is slightly
simpler than the first.

## [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6)

In this small puzzle, the temptation is to engineer an efficient solution,
but for this input a simplistic approach runs quickly enough, even with
the increased window size of the second part.  Working out a version that
is linear in the window size is a nice exercise, but complete overkill
for this problem size.

## [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7)

There's a moderate amount of parsing, but most of the work here is in
two stages, building and traversing the file tree.  The building came out
neatly, and the traversal is a fairly routine recursion.  In the sample
input, all file names are unique, but this is not the case for the full
input.  Because I built a full tree from the start, this had no impact.

In both inputs, the tree is traversed in-order, so no directory is
listed twice.  I didn't rely on that, but it does allow solutions that
build up sizes by recursively consuming the list of commands, which
could be shorter, but probably more complex.

After doing all that for the first part, there's little to do in the
second part, but my first answer was wrong.  After a moment fearing
a bug in the tree building, I read the question more carefully and got
the correct answer.

## [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8)

This was a novel puzzle, which came out very neatly using bulk operations
on lists to exploit the 4-fold symmetry.  The second part shares most
of the structure of the first, encouraging parameterization by simple
functions.

## [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9)

This was another novel puzzle, involving lists and a little geometry.
The first part decomposed quite neatly, and the pieces could be reused
for the second part.

I implemented the dragging operation with more generality than was
needed for the first part, because it came out neatly, and so did not
notice that the second part involved longer moves.  (It just worked.)

## [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10)

This one was a bit frustrating, requiring close reading to get the
precise details right, but ultimately simple code.

## [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11)

This puzzle followed the common Advent of Code pattern were the first
part is understanding the situation and the second scales it up so much
that the naive approach is infeasible, and a deeper understanding of
what is going on is required.  The first part was itself quite intricate,
with complex parsing and incremental changes to the state.  The second
part very clearly says that the size of the numbers is going to be the
problem, but I took too long trying various optimizations, when what is
required is a technique that has featured in several past years.

## [Day 12: Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)

The first part is an easy variation on past problems involving finding
paths in a grid.  For the second part, a brute force search reusing the
first part is fast enough, but a simple variation is faster and neater.

**Lesson:** sometimes it's useful to run path searches backwards.

## [Day 13: Distress Signal](https://adventofcode.com/2022/day/13)

After the parsing, the main work here is coding the comparison function
from the description.  Fortunately the function described is conventional.
The rest is simple list processing.

## [Day 14: Regolith Reservoir](https://adventofcode.com/2022/day/14)

As noted in the problem description, this problem is a variation of
[2018 day 17: Reservoir Research](https://adventofcode.com/2018/day/17).
This version is a bit easier.  Naively tracing each grain of sand worked.
The second part is a little easier, but slower (because more grains).
The whole thing runs for 4 seconds, so there is room for improvement.
