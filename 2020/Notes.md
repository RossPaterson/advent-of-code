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
I added one rather than subtracting on the first attempt.

## [Day 3: Toboggan Trajectory](https://adventofcode.com/2020/day/3)

This was a fun little puzzle, described in terms of a repeating landscape,
but neatly solvable using arithmetic on coordinates.  Solutions using
`repeat` and indexing are shorter, and still feasible for the input size.
