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

## [Day 5: Lanternfish](https://adventofcode.com/2021/day/5)

This puzzle hinges on the choice of data representation.  The description
mentions exponential growth, but also presents a naive representation
that will obviously explode with the extra iterations expected from the
second part.  I started with a more efficient representation (there's
really only one choice), which worked for both parts without change.

I fretted a bit about integer overflow, but it wasn't an issue with
this input.  I could have avoided the issue by using `Integer` in the
first place.
