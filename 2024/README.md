# Notes on each day's solutions

![Completion times 2024](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2024.png)

(image by [Jo Wood](https://github.com/jwoLondon))

## [Day 1: Historian Hysteria](https://adventofcode.com/2024/day/1)

This was quite a bit easier than last year's usual starting problem, but
a tad more than previous years.  The two parts are specified elementwise,
but have neat implementations using bulk operations on lists and maps
respectively.

## [Day 2: Red-Nosed Reports](https://adventofcode.com/2024/day/2)

This was simple list manipulation.  The second part looks more
complicated, but yields to a simple exhaustive search.

## [Day 3: Mull It Over](https://adventofcode.com/2024/day/3)

This was a job for regular expressions, with the second part also needing
a little list manipulation.

## [Day 4: Ceres Search](https://adventofcode.com/2024/day/4)

I initially treated this as a list manipulation, which was adequate for
the first part, but not for the twist in the second.  Both parts are
easily done using positions in a grid.

## [Day 5: Print Queue](https://adventofcode.com/2024/day/5)

The first part is an easy list manipulation that can be done without
relying on any properties of the input.  The second part is sneaky.
The test input has two properties that could make this easier, but the
second is not shared by the puzzle input, even though the wording seems
to implicitly assume that it does.  Once you get past that, the actual
implementation is simple.
