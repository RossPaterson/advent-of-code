# Notes on each day's solutions

From this year, there are only 12 days of puzzles, and no more global
leaderboard (ruined by AI abuse).  Thus there is no easy comparison
of relative difficulty.

I have added difficulty ratings based on how long I took to solve
each puzzle.  The most fun so far have been days 3 and 5.

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
