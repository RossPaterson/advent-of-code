# Notes on each day's solutions

![Completion times 2023](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2023.png)

(image by [Jo Wood](https://github.com/jwoLondon))

This year started off a little tougher than the last 3 years, but there
were still several easy ones.  There was some foreshadowing in the
early puzzles (e.g between days 5 and 16, days 8 and 20, days 10 and 18,
and days 9 and 21).

Some puzzles required, or were much easier with, prior knowledge of
algorithms (days 12, 14, 17 and 23) or mathematics (days 10, 18 and 24).
These things are good to know, but this means the leaderboard times for
these days underestimate the relative difficulty for less expert solvers.

I have added difficulty ratings based on how long I took to solve each puzzle.

## [Day 1: Trebuchet?!](https://adventofcode.com/2023/day/1) \*

This was a bit more work than the usual starting problem.  The second
part gives a taste of our initial solution working for the test input
but not the real input.  The test input does at least suggest (on every
second line) the question we should be asking, but it also contains a
red herring.

## [Day 2: Cube Conundrum](https://adventofcode.com/2023/day/2) \*

The main work here is parsing the input.  After that, the first part is
a simple computation and the second part is the inverse.

## [Day 3: Gear Ratios](https://adventofcode.com/2023/day/3) \*

The main effort here is a bit of 2-dimensional parsing, but the rest is
straightforward if you follow the definitions precisely.

## [Day 4: Scratchcards](https://adventofcode.com/2023/day/4) \*

The parsing is much the same as day 2.  The first part is simple, but
gets us thinking about powers, and in the second part we clearly need
to change the representation to avoid exponential blowup, but it comes
out neatly.

## [Day 5: If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5) \*\*

This was a step up.  After a bit of parsing, the first part sets the
stage, while the second part requires a shift to ranges.

## [Day 6: Wait For It](https://adventofcode.com/2023/day/6) \*

A much easier puzzle.  The second part can be done with an analytic
solution using an old formula from school, but reusing the exhaustive
search of the first part runs in a fraction of a second.

## [Day 7: Camel Cards](https://adventofcode.com/2023/day/7) \*\*

This was about accurately implementing intricate rules.  In the first
part, I implemented the standard ordering, before noticing that this
version is simpler.  The second part is trickier, but exhaustive search
was fast enough.

## [Day 8: Haunted Wasteland](https://adventofcode.com/2023/day/8) \*

The first part was an easy setup.  The second part will be infeasible
unless the supplied inputs have some repeating pattern.  A little
experimentation reveals that the repeat is particularly simple.

## [Day 9: Mirage Maintenance](https://adventofcode.com/2023/day/9) \*

A little problem of integer sequences, based on the method of finite
differences.  The two parts have very neat matching solutions using list
functions that directly implement the description.  The Reddit discussion
mentions an even easier way to exploit the symmetry.

## [Day 10: Pipe Maze](https://adventofcode.com/2023/day/10) \*\*\*

This was novel and tricky.  To parse the input we need to determine what
piece lies at the start position, which takes a while, but sets us up
for following the pipe in the first part.  The second part can be done
with a standard algorithm, but we have to work out how to fit it to the
representation used here.  Fortunately there are enough test cases to
catch errors in the logic.

## [Day 11: Cosmic Expansion](https://adventofcode.com/2023/day/11) \*

This was a cute little problem of expanding lists.  I went for a
mapping solution in the first part, and this generalized directly for
the second part.

## [Day 12: Hot Springs](https://adventofcode.com/2023/day/12) \*\*\*

I did the first part by exhaustive search, even though it seemed
likely that the problem would be expanded in the second, as it was.
This required dynamic programming, which is neatly achieved by memoizing
the recursive definition.  I used a two-stage approach.  More compact
keys would have sped things up, but it's fast enough.  Not reading the
definition of unfolding carefully cost some time.

## [Day 13: Point of Incidence](https://adventofcode.com/2023/day/13) \*\*

As often, I started by trying an over-engineered solution, this time
using arithmetic on indices (and it would have been correct if I hadn't
messed up the summary function).  However, the problem turned out be
a neat exercise in list manipulation.  The second part used a variant
of the first inside an exhaustive search, but linear in the size of the
input, and thus quite quick.

## [Day 14: Parabolic Reflector Dish](https://adventofcode.com/2023/day/14) \*\*

The first part is a novel little 2-dimensional problem.  Though it
used north, it was neater to generalize to any direction.  The second
part is clearly infeasible unless there is a cycle.  Having met a few
of these before, I had already written a cycle finder, which did the
trick here too.

## [Day 15: Lens Library](https://adventofcode.com/2023/day/15) \*\*

The first part was almost trivial.  The second was a fairly tedious
exercise in implementing a detailed specification using list and map
manipulation.

## [Day 16: The Floor Will Be Lava](https://adventofcode.com/2023/day/16) \*

This was a novel variation.  There's a bit of detail in getting the
actions of the mirrors and splitters right, but after that this is
another state search.  An exhaustive search repeating the first part
was fast enough for the second part.

## [Day 17: Clumsy Crucible](https://adventofcode.com/2023/day/17) \*\*\*

It took a while to realize what this was, but then it is easy if you
have the necessary library function to hand.  The second part is easily
done by generalizing the first.

## [Day 18: Lavaduct Lagoon](https://adventofcode.com/2023/day/18) \*\*\*

This was novel and difficult.  The first part was similar to day 10, but
the second scaled things up so that discrete approaches no longer worked.
I first solved it by cutting the zone into strips, as in
[this Reddit post](https://www.reddit.com/r/adventofcode/comments/18l6tlj/2023_day_18_developed_my_own_algorithm/),
but this was very ugly.  As seen in the
[Reddit discussion](https://www.reddit.com/r/adventofcode/comments/18l0qtr/2023_day_18_solutions/),
it is much simpler to use a bit of maths that was new to me.

## [Day 19: Aplenty](https://adventofcode.com/2023/day/19) \*\*

This was the first assembly-like problem this year.  As on day 5, the
second part required a shift to collections of ranges.

## [Day 20: Pulse Propagation](https://adventofcode.com/2023/day/20) \*\*\*\*

This was the most difficult so far.  For the first part we need to
simulate a network of flipflops and NAND gates, with curious sequential
behaviour.  For the second part, simulation is infeasible, and we need
to find a repeat (similar to day 8).  The network appears to be designed
so that the key nodes are reset before the end of the iteration, so I
was unable to see the repetition, and disassembled the network by hand.
Generating a display of the network would have have helped.  It turns
out to be a group of modular counters, which is interesting.

## [Day 21: Step Counter](https://adventofcode.com/2023/day/21) \*\*\*\*

This one was even harder (and very novel).  The first part is fairly
easy, and introduces the idea of parity.  In the second part, with
infinite repetitions, the naive implementation is too slow.  I spent
a while thinking about general solutions, which seem very difficult,
before looking at the actual input.  This has a special property
that greatly simplifies the problem.  The sample input lacks this
property, which is sneaky.  Still, the examples are useful, because
the efficient implementation is hard to get right, so we have to do
the naive implementation, test it with the examples given, and then
compare our faster version with this one on small inputs that have the
simplifying property.

From the [Reddit thread](https://www.reddit.com/r/adventofcode/comments/18nevo3/2023_day_21_solutions/),
the input and the given number of steps are even sneakier, so that there
is a simple formula for the answer (recalling day 9).  So that's why
the input had that big diamond shape in it.

## [Day 22: Sand Slabs](https://adventofcode.com/2023/day/22) \*\*

A nice little exercise in universal and existential quantifiers.
My first version, using lists, was a little slow, but got the answer.
It can be sped up neatly using containers.

## [Day 23: A Long Walk](https://adventofcode.com/2023/day/23) \*\*\*

The first part is easy enough with exhaustive search, but removing the
one-way gates in the second part yields a longest path problem, which
is NP-hard in general.  However, it can be solved by converting it to
a weighted graph and using a branch-and-bound search on that.

## [Day 24: Never Tell Me The Odds](https://adventofcode.com/2024/day/24) \*\*\*\*

The first part requires some simple linear algebra, but uses rays rather
than lines.  The second part involves quadratic terms, but they can be
eliminated, leaving a system of linear equations.

## [Day 25: Snowverload](https://adventofcode.com/2025/day/25) \*

This was an easy one to finish with, if we use a tool to visualize
the graph.
