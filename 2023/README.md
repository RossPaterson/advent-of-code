# Notes on each day's solutions

![Completion times 2023](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2023.png)

(image by [Jo Wood](https://github.com/jwoLondon))

Some of the early puzzles (days 3, 5 and 7) were significantly tougher
than in the last 3 years.

The narrative appears to be a descending recursion through the islands,
through which we will return later.

## [Day 1: Trebuchet?!](https://adventofcode.com/2023/day/1)

This was a bit more work than the usual starting problem.  The second
part gives a taste of our initial solution working for the test input
but not the real input.  The test input does at least suggest (on every
second line) the question we should be asking, but it also contains a
red herring.

## [Day 2: Cube Conundrum](https://adventofcode.com/2023/day/2)

The main work here is parsing the input.  After that, the first part is
a simple computation and the second part is the inverse.

## [Day 3: Gear Ratios](https://adventofcode.com/2023/day/3)

The main effort here is a bit of 2-dimensional parsing, but the rest is
straightforward if you follow the definitions precisely.

## [Day 4: Scratchcards](https://adventofcode.com/2023/day/4)

The parsing is much the same as day 2.  The first part is simple, but
gets us thinking about powers, and in the second part we clearly need
to change the representation to avoid exponential blowup, but it comes
out neatly.

## [Day 5: If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)

This was a step up.  After a bit of parsing, the first part sets the
stage, while the second part requires a shift to ranges.

## [Day 6: Wait For It](https://adventofcode.com/2023/day/6)

A much easier puzzle.  The second part can be done with an analytic
solution using an old formula from school, but reusing the exhaustive
search of the first part runs in a fraction of a second.

## [Day 7: Camel Cards](https://adventofcode.com/2023/day/7)

This was about accurately implementing intricate rules.  In the first
part, I implemented the standard ordering, before noticing that this
version is simpler.  The second part is trickier, but exhaustive search
was fast enough.

## [Day 8: Haunted Wasteland](https://adventofcode.com/2023/day/8)

The first part was an easy setup.  The second part will be infeasible
unless the supplied inputs have some repeating pattern.  A little
experimentation reveals that the repeat is particularly simple.

## [Day 9: Mirage Maintenance](https://adventofcode.com/2023/day/9)

A little problem of integer sequences.  The two parts have very neat
matching solutions using list functions that directly implement the
description.  The Reddit discussion mentions an even easier way to
exploit the symmetry.

## [Day 10: Pipe Maze](https://adventofcode.com/2023/day/10)

This was novel and tricky.  To parse the input we need to determine what
piece lies at the start position, which takes a while, but sets us up
for following the pipe in the first part.  The second part can be done
with a standard algorithm, but we have to work out how to fit it to the
representation used here.  Fortunately there are enough test cases to
catch errors in the logic.

## [Day 11: Cosmic Expansion](https://adventofcode.com/2023/day/11)

This was a cute little problem of expanding lists.  I went for a
mapping solution in the first part, and this generalized directly for
the second part.

## [Day 12: Hot Springs](https://adventofcode.com/2023/day/12)

I did the first part by exhausive search, even though it seemed
likely that the problem would be expanded in the seconrt, as it was.
This required dynamic programming, which is neatly achieved by memoizing
the recursive definition.  I used a two-stage approach.  More compact
keys would have sped things up, but it's fast enough.  Not reading the
definition of unfolding carefully cost some time.

## [Day 13: Point of Incidence](https://adventofcode.com/2023/day/13)

As often, I started by trying an overengineered solution, this time using
arithmetic on indices (and it would have been correct if I hadn't messed
up the summary function).  However, the problem turned out be a neat
exercise in list manipulation.  The second part used a variant of the
first inside an exhaustive search, but linear in the size of the input,
and thus quite quick.
