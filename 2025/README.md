# Notes on each day's solutions

I have added difficulty ratings based on how long I took to solve
each puzzle.

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

This was more list processing.  The first part with easy, but using
the same naive approach for the second part was infeasible.  Dynamic
programming works well here, and there is a neat way to express it.

As pointed out on the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/1pcvaj4/2025_day_3_solutions/),
the problem has properties that also permit a greedy solution.  This can
also be expressed neatly.
