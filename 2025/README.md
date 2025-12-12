# Notes on each day's solutions

From this year, there are only 12 days of puzzles, and no more global
leaderboard (ruined by AI abuse).  Thus there is no easy comparison
of relative difficulty.

I have added difficulty ratings based on how long I took to solve
each puzzle.  The most fun were days 3, 5 and 9.

## [Day 1: Secret Entrance](https://adventofcode.com/2025/day/1) \*

This was a bit more than previous openers, particularly in the second
part, but ultimately a small exercise in sequential list processing,
with lots of tricky edge cases.  The second part triggers features of
the puzzle input that are not present in the test input, but this is
clearly signposted in the puzzle description.

## [Day 2: Gift Shop](https://adventofcode.com/2025/day/2) \*

This was another small list processing exercise.  There are several
strategies for doing this efficiently, but the naive brute force approach
is fast enough.

## [Day 3: Lobby](https://adventofcode.com/2025/day/3) \*

This was more list processing.  The first part was easy, but using
the same naive approach for the second part was infeasible.  I used
dynamic programming, and there is a neat way to express it.

As pointed out on the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/1pcvaj4/2025_day_3_solutions/),
the problem has properties that also permit a greedy solution.  This can
also be expressed neatly.

## [Day 4: Printing Department](https://adventofcode.com/2025/day/4) \*

This was a straightforward two-dimensional grid problem, with the second
part an iteration of the first.

## [Day 5: Cafeteria](https://adventofcode.com/2025/day/5) \*

More list processing.  The first part is trivial, but the second obviously
needs a different approach.  The final code is small and neat.

## [Day 6: Trash Compactor](https://adventofcode.com/2025/day/6) \*

In this problem, main challenge in the second part is parsing the input
in a different way.  It was a little fiddly, but there is little choice
in method.

## [Day 7: Laboratories](https://adventofcode.com/2025/day/7) \*

The straightforward first part sets up the problem, but naively
implementing the description of the second part is clearly infeasible.
Instead, the return of an idea seen previously (e.g. 2021 days 6 and 14,
2024 day 11) yields a solution that is a simple variation on part one.

## [Day 8: Playground](https://adventofcode.com/2025/day/8) \*

The two parts are variants of the same computation.  I was confused by
the ambiguity of "nothing happens", as were many people commenting on
Reddit.  This hints at Kruskal's algorithm for minimal spanning trees,
but a naive implementation is sufficient.

## [Day 9: Playground](https://adventofcode.com/2025/day/9) \*\*

One of those with an easy first part to set things up and a much more
difficult second part.  It took me several false starts and much discarded
code to discover the simplicity of this one, though my solution does
rely on the input not containing a particular special case.

## [Day 10: Factory](https://adventofcode.com/2025/day/10) \*\*\*\*

This was a striking increase in difficulty from previous days.  The two
parts are Boolean and natural number versions of a linear system.
Although we have seen something similar previously (2023 day 24), this
system was not fully determined.

## [Day 11: Reactor](https://adventofcode.com/2025/day/11) \*\*

This is a graph problem where the naive method for the first part is
correct but infeasible for the second.  It wasn't clear what kind of graph
we were dealing with, and the description has a red herring "in any order".
Visualizing the graph showed that this was a problem of familiar type,
for which there is a standard approach.

## [Day 12: Christmas Tree Farm](https://adventofcode.com/2025/day/12) \*

This was quite sneaky.  The problem looks completely infeasible, so
before trying an exhaustive search, I checked whether how much of the
actual input was trivial.
