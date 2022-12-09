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
