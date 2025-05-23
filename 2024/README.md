# Notes on each day's solutions

![Completion times 2024](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2024.png)

(image by [Jo Wood](https://github.com/jwoLondon))

The early graphs are not as representative this year, as the global
leaderboard is infested with LLM users.  Some puzzles required, or were
much easier with, prior knowledge of algorithms (days 16, 19 and 23)
or mathematics (day 13), which means the leaderboard times for these
days underestimate the relative difficulty for less expert solvers.
I have added difficulty ratings based on how long I took to solve
each puzzle.

Day 14 was a fun hunt, and day 21 a real mindbender.
Days 6, 9, 12 and 15 were also particularly neat ideas.

## [Day 1: Historian Hysteria](https://adventofcode.com/2024/day/1) \*

This was quite a bit easier than last year's usual starting problem, but
a tad more than previous years.  The two parts are specified elementwise,
but have neat implementations using bulk operations on lists and maps
respectively.

## [Day 2: Red-Nosed Reports](https://adventofcode.com/2024/day/2) \*

This was simple list manipulation.  The second part looks more
complicated, but yields to a simple exhaustive search.

## [Day 3: Mull It Over](https://adventofcode.com/2024/day/3) \*

This was a job for regular expressions, with the second part also needing
a little list manipulation.

## [Day 4: Ceres Search](https://adventofcode.com/2024/day/4) \*\*

I initially treated this as a list manipulation, which was adequate for
the first part, but not for the twist in the second.  Both parts are
easily done using positions in a grid.

## [Day 5: Print Queue](https://adventofcode.com/2024/day/5) \*\*

The first part is an easy list manipulation that can be done without
relying on any properties of the input.  The second part is sneaky.
The test input has two properties that could make this easier, but the
second is not shared by the puzzle input, even though the wording seems
to implicitly assume that it does.  Once you get past that, the actual
implementation is simple.

## [Day 6: Guard Gallivant](https://adventofcode.com/2024/day/6) \*

This was a neat little exercise with lists and sets, with most of the
first part reusable for an exhaustive search in the second.  My first
version was completely naive, but still obtained the answer in 36 seconds.

## [Day 7: Bridge Repair](https://adventofcode.com/2024/day/7) \*

The first part is a straightforward exhaustive search, with the second
a small extension.  Doing the calculations in reverse would produce
a substantian speedup, but the simpler forwards approach handles both
parts in well under a second.

I misread the second part, solved a much more difficult problem, and
then wondered why the examples were "wrong".

## [Day 8: Resonant Collinearity](https://adventofcode.com/2024/day/8) \*

A fairly simple little exercise in decomposition using lists and sets.
The second part is an iterative variant of the first.

## [Day 9: Disk Fragmenter](https://adventofcode.com/2024/day/9) \*\*

The silliness of the compaction algorithm specified is quite distracting,
but this was a more difficult list manipulation, traversing from both
ends and meeting in the middle.  This can be done with a deque, but it
is cleaner to split the list into two lists, which are each processed
sequentially (but differently).  The second part changes the processing,
but has a similar structure.  The naive quadratic approach is fast
enough on this input, but could be sped up using a priority queue.
Both parts involve multiple cases with fiddly details.

## [Day 10: Hoof It](https://adventofcode.com/2024/day/10) \*

This was an easier exercise involving searching in a directed acyclic
graph.  The second part is a minor variant of the first.

## [Day 11: Plutonian Pebbles](https://adventofcode.com/2024/day/11) \*

The first part establishes a simple repetitive process, while the second
scales the number of repetitions well beyond the point where a naive
implementation of the description becomes infeasible.  A small insight
into the nature of the process is required to produce a neat and fast
implementation using Map.

## [Day 12: Garden Groups](https://adventofcode.com/2024/day/12) \*\*

This was a larger problem, with a spatial flavour and the two parts
exploring different aspects.  Several examples are provided.

For the second part, my original solution, following the description and
the lead of the first part, was quite heavy-weight.  However, as pointed
out on
[this Reddit thread](https://www.reddit.com/r/adventofcode/comments/1hcf16m/2024_day_12_everyone_must_be_hating_today_so_here/),
an elementary combinatorial observation leads to a much simpler solution.

## [Day 13: Claw Contraption](https://adventofcode.com/2024/day/13) \*\*

This puzzle is numerical, with no data structures involved.  Again the
second part scales up the first so that a direct implementation of the
description is infeasible.  It took me a while to realize how simple
it was.

## [Day 14: Restroom Redoubt](https://adventofcode.com/2024/day/14) \*\*

This is a pretty puzzle using a simple particle system (a variation
on 2018 day 10).  The measure in the first part is a bit silly, but
checks correct implementation of the system, as well as suggesting the
idea of applying a metric to the image.  The loose specification of the
second part turns this into a data science challenge.  A wide variety
of approaches can be used.

My first guess at what the shape would look like was wrong, but it
did drastically narrow down the number of images to visually inspect.
Then I reused some code from day 12 to identify the unusual frame.
There are also approaches making use of the independent periodicity of
the x and y coordinates.

## [Day 15: Warehouse Woes](https://adventofcode.com/2024/day/15) \*\*\*

This task was an implementation of two elaborations of the Sokoban game.
It did not require any special insights, just painstaking attention to
implementing the instructions while covering all cases, especially in the
second part.  The test input provided was large enough to find my bugs.

## [Day 16: Reindeer Maze](https://adventofcode.com/2024/day/16) \*\*\*

The first part is a straightforward shortest path problem, requiring
only the length, while the second requires a statistic on all the
shortest paths.  As with many AoC puzzles, once you find the key
optimization, all the smaller optimizations are unnecessary.

## [Day 17: Chronospatial Computer](https://adventofcode.com/2024/day/17) \*\*\*

The first part is implementing a small abstract machine and checking
that it works.  The second part requires us to determine the start
state from the output.  This involves some disassembly and analysis of
the computation that the program is performing.

## [Day 18: RAM Run](https://adventofcode.com/2024/day/18) \*

A simple maze search was a relief after a few tough days.  For the second
part, iteratively repeating the first part was slow, but fast enough.
A binary search is much faster.

## [Day 19: Linen Layout](https://adventofcode.com/2024/day/19) \*\*

The naive approach to the first part is too slow.  There are two
approaches to speeding it up, only one of which generalizes to the
second part.

## [Day 20: Race Condition](https://adventofcode.com/2024/day/20) \*\*

Another maze, with a second part a fairly straightforward generalization
of the first.  The puzzle states that there is a single path from start
to finish, and the inputs have no forks, but handling a general maze
isn't much more difficult.

## [Day 21: Keypad Conundrum](https://adventofcode.com/2024/day/21) \*\*\*\*

This was an ingenious puzzle with a considerable conceptual challenge.
The first part can be done with an exhaustive search, but the second
part scales it up so much that one must derive an inductive definition.
Once the appropriate inductive structure is identified, the solution
comes out neatly, but it took quite a while to get that understanding.

## [Day 22: Monkey Market](https://adventofcode.com/2024/day/22) \*\*

This was a bit of a rest after the previous day.  The first part was
easy, and the second involved some straightforward bulk operations.
This puzzle was unusual in that local optimizations are needed to get
a reasonable runtime.

## [Day 23: LAN Party](https://adventofcode.com/2024/day/23) \*\*\*

The maximum clique problem is NP-complete in the general case, but
this input yields to a wide variety of approaches.  I learned and used
a general algorithm, but the input graph has several properties that
could be exploited to find solutions quickly.

## [Day 24: Crossed Wires](https://adventofcode.com/2024/day/24) \*\*\*

The puzzle is to execute and then find errors in a gate-level circuit
(a ripple-carry adder).  Although the description talks about finding
4 pairs of interchanged labels, the requested answer requires only the
8 labels involved.

Drawing the graph with Graphviz helped map out the intended circuit.
Comparison of the input data with the expected local connections of
each gate yielded 7 clear errors and a few candidates for the 8th.
My second guess succeeded.

## [Day 25: Code Chronicle](https://adventofcode.com/2024/day/25) \*

The last puzzle was an easy bulk list manipulation.
