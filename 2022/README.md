# Notes on each day's solutions

![Completion times 2022](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2022.png)

(image by [Jo Wood](https://github.com/jwoLondon))

This year had several quite difficult problems, but thankfully mixed in
with easier ones.

Some recurring themes:
tree structures (days 7 and 21),
simple searches (days 12 and 18),
search optimization problems (days 16 and 19),
spatial reasoning (days 15 and 22).
Some problems were easier versions of puzzles from previous years
(days 12, 14 and 20).
The most novel were days 8, 9, 16, 17 and 22.

I have added difficulty ratings based on how long I took to solve each puzzle.

## [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1) \*

This was an easy exercise with list functions, directly implementing
the description.

## [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2) \*

This involves simple functions on values.  It shouldn't have been
difficult, but I got in a tangle mixing up the four 3-value types that
were involved.

## [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3) \*

An easy exercise using lists and sets.

## [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4) \*

This one involves a little parsing, and then basic range operations,
with a neat symmetry between the parts.

## [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5) \*

After messy input parsing, the main computation is list processing,
which works out neatly using list functions.  The second part is slightly
simpler than the first.

## [Day 6: Tuning Trouble](https://adventofcode.com/2022/day/6) \*

In this small puzzle, the temptation is to engineer an efficient solution,
but for this input a simplistic approach runs quickly enough, even with
the increased window size of the second part.  Working out a version that
is linear in the window size is a nice exercise, but complete overkill
for this problem size.

## [Day 7: No Space Left On Device](https://adventofcode.com/2022/day/7) \*\*

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

## [Day 8: Treetop Tree House](https://adventofcode.com/2022/day/8) \*

This was a novel puzzle, which came out very neatly using bulk operations
on lists to exploit the 4-fold symmetry.  The second part shares most
of the structure of the first, encouraging parameterization by simple
functions.

## [Day 9: Rope Bridge](https://adventofcode.com/2022/day/9) \*\*

This was another novel puzzle, involving lists and a little geometry.
The first part decomposed quite neatly, and the pieces could be reused
for the second part.

I implemented the dragging operation with more generality than was
needed for the first part, because it came out neatly, and so did not
notice that the second part involved longer moves.  (It just worked.)

## [Day 10: Cathode-Ray Tube](https://adventofcode.com/2022/day/10) \*

This one was a bit frustrating, requiring close reading to get the
precise details right, but ultimately simple code.

## [Day 11: Monkey in the Middle](https://adventofcode.com/2022/day/11) \*\*\*

This puzzle followed the common Advent of Code pattern were the first
part is understanding the situation and the second scales it up so much
that the naive approach is infeasible, and a deeper understanding of
what is going on is required.  The first part was itself quite intricate,
with complex parsing and incremental changes to the state.  The second
part very clearly says that the size of the numbers is going to be the
problem, but I took too long trying various optimizations, when what is
required is a technique that has featured in several past years.

## [Day 12: Hill Climbing Algorithm](https://adventofcode.com/2022/day/12) \*

The first part is an easy variation on past problems involving finding
paths in a grid.  For the second part, a brute force search reusing the
first part is fast enough, but a simple variation is faster and neater.

**Lesson:** sometimes it's useful to run path searches backwards.

## [Day 13: Distress Signal](https://adventofcode.com/2022/day/13) \*

After the parsing, the main work here is coding the comparison function
from the description.  Fortunately the function described is conventional.
The rest is simple list processing.

## [Day 14: Regolith Reservoir](https://adventofcode.com/2022/day/14) \*\*

As noted in the problem description, this problem is a variation of
[2018 day 17: Reservoir Research](https://adventofcode.com/2018/day/17).
This version is a bit easier.  Naively tracing each grain of sand worked.
The second part is a little easier, but slower (because more grains).
The whole thing runs for 4 seconds, so there is room for improvement.

## [Day 15: Beacon Exclusion Zone](https://adventofcode.com/2022/day/15) \*\*\*\*

I find spatial problems like this quite difficult.  After getting an
answer to the first part, I sped it up by using intervals, and it was
natural to try the two-dimensional counterpart in the second part, but
it took me ages to get the difference operation correct.  Separating
out a diagonal square lattice abstraction helped, but it's still ugly
(but fast).

## [Day 16: Proboscidea Volcanium](https://adventofcode.com/2022/day/16) \*\*\*

This is a novel mix of path-finding and optimization, where the challenge
is finding a feasible implementation.  I got the first part with a direct
implementation and over half an hour of runtime, but then was fortunate
to have to stop and commute to work, giving time to step back and devise
a multistage solution to both parts (which still took 40 seconds to run).

Often an Advent of Code problem is presented in terms of how the state
changes on each clock tick, and that can usually be directly implemented,
at least for small problem sizes.  For a more efficient and scalable
solution, often one must look at the system in a different way,
e.g. considering the histories of components of the system.

## [Day 17: Pyroclastic Flow](https://adventofcode.com/2022/day/17) \*\*\*

This problem is a variation on the game of Tetris.  A naive direct
implementation sufficed for the first part.  It was clear the second part
would be infeasible unless the behaviour was periodic.  The period for the
given input could be determined by inspecting a trace of the execution,
which I did manually.

## [Day 18: Boiling Boulders](https://adventofcode.com/2022/day/18) \*

This was a neat little exercise in set manipulation, and a welcome
respite after three gruelling days.  The easy first part sets the scene.
For the second part, I initially focussed on the trapped pockets, which
seemed daunting, but the fourth sentence of the description translates
directly into an implementation (once one limits the search area)
that can be composed with the function created for the first part.

## [Day 19: Not Enough Minerals](https://adventofcode.com/2022/day/19) \*\*\*\*

This was a tough problem, reminiscent of day 16 but more difficult.
It was also unusual in that the first part was infeasible without constraining
the search space.  From examination of the input I managed to come up
with a constraint on the number of robots.  The second part was larger,
and I needed a hint from the [Reddit thread](https://www.reddit.com/r/adventofcode/comments/zpihwi/2022_day_19_solutions/) for that.

**Lesson:** depth-first search with a heuristic bound can drastically
reduce the search space.  It can also be decoupled quite neatly in a
lazy language.

## [Day 20: Grove Positioning System](https://adventofcode.com/2022/day/20) \*\*

This one looks like it will need a sophisticated data structure, like
[2020 day 23: Crab Cups](https://adventofcode.com/2020/day/23).
Perhaps the second part will ask us to do the process a billion times,
or to invert it?  But no, it turns out that simple list manipulation is
fast enough for both parts, once we get the modular arithmetic right.

## [Day 21: Monkey Math](https://adventofcode.com/2022/day/21) \*\*

This was another moderate problem, requiring a fair amount of
straightforward code.  The first part is routine expression evaluation.
The second part requires solving an equation.  The general case would be
infeasible, so the input must have some special form.  Part of the work
is a standard exercise in recursive traversal, after which one can see
that the input is indeed of a form that can be solved easily.

## [Day 22: Monkey Map](https://adventofcode.com/2022/day/22) \*\*\*\*

This was the most difficult so far.  The first part is finicky but
conceptually easy, and sets the scene.  The cube folding in the second
part is very tough, especially those like me who find spatial reasoning
difficult, even with scissors and paper.  In addition, the the sample
input and real input had different shapes.  I went for a (very ugly)
general solution, but most people in the
[Reddit thread](https://www.reddit.com/r/adventofcode/comments/zsct8w/2022_day_22_solutions/)
hardcoded the cube.

## [Day 23: Unstable Diffusion](https://adventofcode.com/2022/day/23) \*\*

Another welcome respite, this problem is neatly expressed using bulk
operations on sets and maps.  The main issue with the first part was
reading all the instructions.  The second part is a trivial extension
if the first part was properly factored.

## [Day 24: Blizzard Basin](https://adventofcode.com/2022/day/24) \*\*

Once one gets the basic insight necessary for efficient implementation,
this was an exercise in bulk operations on containers, though with lots
of opportunities for off-by-one errors.  The second part requires a
little refactoring of the first.
