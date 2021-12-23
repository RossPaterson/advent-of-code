# Notes on each day's solutions

![Completion times 2021](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2021.png)

(image by [Jo Wood](https://github.com/jwoLondon))

## [Day 1: Sonar Sweep](https://adventofcode.com/2021/day/1)

This was an easy exercise with list functions, especially `zip`.
I did the second part as a direct implementation of the description,
but a little analysis yields a more efficient solution.

## [Day 2: Dive!](https://adventofcode.com/2021/day/2)

This was an exercise in applying a list of commands to a state, as seen
in early puzzles in previous years.

## [Day 3: Binary Diagnostic](https://adventofcode.com/2021/day/3)

The description (particularly the second part) is lengthy, but the first
part is a neat list manipulation, while the second requires recursion.
In each part, using two separate traversals is much easier than trying
to do both at once.  Both parts rely on unstated properties of the input.

## [Day 4: Giant Squid](https://adventofcode.com/2021/day/4)

The bingo game is a moderately involved sequential computation.  I fixed
on a convenient representation early, but then flailed around a bit and
also didn't read the scoring function properly.  In the end the solution
is quite clean.

## [Day 5: Hydrothermal Venture](https://adventofcode.com/2021/day/5)

A somewhat easier puzzle, requiring a little parsing and then some
processing of 2-dimensional coordinates.  The extension in the second part
is mostly implicit in the description of the first.  My main stumbling
block in both parts was with getting the right ranges of numbers.

## [Day 6: Lanternfish](https://adventofcode.com/2021/day/6)

This puzzle hinges on the choice of data representation.  The description
mentions exponential growth, but also presents a naive representation
that will obviously explode with the extra iterations expected from the
second part.  I started with a more efficient representation (there's
really only one choice), which worked for both parts without change.

I fretted a bit about integer overflow, but it wasn't an issue with
this input.  I could have avoided the issue by using `Integer` in the
first place.

## [Day 7: The Treachery of Whales](https://adventofcode.com/2021/day/7)

This one was quite a bit simpler.  In contrast to yesterday's
resource-sensitive puzzle, this time a direct exhaustive search was
sufficient.  There was only a slightly more complex cost function in
the second part (a little basic maths helps here).

From the Reddit thread, the minima for the two parts occur at the median
and near the mean respectively.  On the other hand, the exhaustive search
ran in 85 ms.

## [Day 8: Seven Segment Search](https://adventofcode.com/2021/day/8)

This puzzle had an intricate description.  The first part is easy
enough, but I made quite a meal of the second part, partly by not fully
understanding the description, and partly by implementing a general
constraint-solving approach to get the wire-segment and display-digit
mappings, which was quite tricky to get right.  It is much simpler
to deduce the display-digit mapping directly using a traditional
puzzle-solving approach, which amounts to operating on sets instead
of elements.  Part one hinted at this approach, but the description of
part two dwelled on the wire-segment mapping.

## [Day 9: Smoke Basin](https://adventofcode.com/2021/day/9)

In the end, this is a fairly simple graph puzzle.  The second part is a
lot simpler than it first appeared, due to restrictions on the input that
were given in the description.  Before I realized that, I had written
a fair bit of unnecessary code.

## [Day 10: Syntax Scoring](https://adventofcode.com/2021/day/10)

This bracket-watching task was easier than the last few days, with little
scope for different solutions.

## [Day 11: Dumbo Octopus](https://adventofcode.com/2021/day/11)

Another fairly easy puzzle, consisting of a novel cellular automaton.
I found it a nice exercise with maps, but procedural solutions with
arrays have similar complexity.  The second part is a different query
on the same sequence of counts.

As noted on the Reddit thread, this is a
[model of firefly synchronization](http://dx.doi.org/10.1007/978-3-319-78512-7_8)
([preprint](https://www.researchgate.net/publication/325791357)).

## [Day 12: Passage Pathing](https://adventofcode.com/2021/day/12)

This puzzle involved two novel variations on path-finding.  There are
no pairs of adjacent big caves in the input, which isn't stated in the
description, but is necessary for the task to make sense.

I optimized the calculation by transforming the original graph to
eliminate the big caves, which speeds things up considerably.  From the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/rehj2r/2021_day_12_solutions/),
this seems not to have been necessary for the small graphs we are given.

## [Day 13: Transparent Origami](https://adventofcode.com/2021/day/13)

This was a nice little exercise in `foldl` and containers, with a bit
of simple geometry.

## [Day 14: Extended Polymerization](https://adventofcode.com/2021/day/14)

Another case where the second part asks us to scale up a calculation
for which the naive implementation uses exponential space and time,
which will obviously be infeasible.  I did this by focussing on what
happens inside each pair, which was a nice exercise with bulk containers
and respectably fast, but several people on the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/rfzq6f/2021_day_14_solutions/)
kept track of the number of occurrences of each pair instead, which is
a bit simpler and faster.

## [Day 15: Chiton](https://adventofcode.com/2021/day/15)

A straight-forward application of
[Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
(previously used on 2015 day 22 and 2019 day 18).  The first part is
[Project Euler Problem 83](https://projecteuler.net/problem=83).
The second part involves a fairly easy expansion.  The same algorithm
is still feasible, though A\* would be faster.

## [Day 16: Packet Decoder](https://adventofcode.com/2021/day/16)

This one was fiddly, without any need for invention.  The first part
was implementation of a very convoluted specification of a recursive
bit sequence decoder.  A state monad was handy here.  Once the input
was converted to a recursive datatype, the rest of the first part and
all of the second part were just recursive reduction.

## [Day 17: Trick Shot](https://adventofcode.com/2021/day/17)

This is superficially a particle system, but we need to work out
mathematical relationships (cf 2017 day 20, 2018 day 10, 2019 day 12).
It's fairly easy to work out enough to devise a feasible exhaustive
search.  A little more analysis makes it quite fast and I stopped there,
but it would be possible to go further.

## [Day 18: Snailfish](https://adventofcode.com/2021/day/18)

This is a complex recursive traversal.  The description is convoluted and
uses confusing terminology.  Once that is understood, the transformation
it describes is also quite complicated, but ultimately comes out as a
fairly clean recursion and application of `Maybe`.  The bulk of the work
is in the first part, with the second part an easy extension reusing
the top-level functions.

## [Day 19: Beacon Scanner](https://adventofcode.com/2021/day/19)

This was tough, partly due to the 3-dimensional geometry and partly
finding a workable approach.  But the problem decomposes neatly into
a series of steps, and an exhaustive search of the possibilities
completes in a few seconds.

It seems that the actual inputs do not trigger the extra tests that
are needed for the general case.

## [Day 20: Beacon Scanner](https://adventofcode.com/2021/day/20)

This was sneaky.  It looks like a simple cellular automaton, and is easy
to make it work for the test input, but the actual input is different.
It is necessary to work in double steps.  Once this is done properly, the
second part is an easy extension, though it takes a few seconds to run.

**Lesson:** identify any assumptions your solution makes about the input
and check whether the actual input satisfies them.

## [Day 21: Dirac Dice](https://adventofcode.com/2021/day/21)

The first part is a simple iterative calculation.  The second can be
expressed recursively and evaluated efficiently using memoization, since
the state space isn't too large.

The time could be cut by a factor of 4 by counting dice combinations
with the same sum.  Another solution in the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/rl6p8y/2021_day_21_solutions/)
uses the fact that the progress of the two players is independent and
identical.

## [Day 22: Reactor Reboot](https://adventofcode.com/2021/day/22)

It is clear from the start that a point counting approach will be enough
for the first part but infeasible for the second.  I used a cuboid splitting
approach like many others, but as noted in the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/)
there is a subtle and much cleaner (but not faster) solution using
signed cuboids.

## [Day 23: Amphipod](https://adventofcode.com/2021/day/23)

Another shortest-path problem, but it is vital to choose a state
representation that identifies equivalent states (as learned on day 11
of 2016), and we know from day 6 that entities with the same attributes
are indistinguishable.  Expressing the moves was a bit finicky, but once
the first part was working, the main difficulty in the second part was
finding all the places where the original size was baked in.
