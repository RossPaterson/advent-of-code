# Notes on each day's solutions

![Completion times 2018](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2018.png)

(image by Jo Wood)

## Day 1: Chronal Calibration

An easy one to start with.  The first part was just a sum, while in
the second part was easiest broken down into three parts:

* expand the inputs by going back to the start when running out, using `cycle`
* generate partial sums, using `scanl`
* look for one that was in the set of values already seen.  This could be
  done by generating a list with the set of values in each prefix, using
  another `scanl`, and comparing that with the list of partial sums.
  We can use these infinite lists thanks to lazy evaluation.

## Day 2: Inventory Management System

The first part was a routine list comprehension, but the second was
an interesting challenge of moderate size.  Rather than compare each
string with each other string, I went straight for generating all the
possible edits and searching for duplicates by sorting them.  That is
asymptotically faster, O(*n log n*) vs O(*n*<sup>2</sup>), but for an
input of this size was slower than the quadratic algorithm.

## Day 3: No Matter How You Slice It

Another small and enjoyable problem.  I used frequency counting again
(perhaps because I'd used it the day before), this time of the squares
in each claim, and it did the job.

The second part was an easy extension, partly reusing the first part,
and partly inverting it.

## Day 4: Repose Record

This one wasn't difficult, but there was some tedious processing of the
input before one can start.  Then the two parts are different calculations
on that data.  The bulk operations on maps were useful here.

## Day 5: Alchemical Reduction

This was fun, and immediately recognizable as normalization in a
[free group](https://en.wikipedia.org/wiki/Free_group).
The key insight is that you can build up a reduced string by adding
one element at a time.  I did it with a `foldl`, which left the result
in the reverse order (though it didn't matter for this puzzle), but it
could also be done with a `foldr`.

The second part is a straightforward comparison of all the possible
removals, but normalizing the string first makes it faster.

## Day 6: Chronal Coordinates

The first part was a kind of
[Voronoi diagram](https://en.wikipedia.org/wiki/Voronoi_diagram),
which I'd encountered, but didn't know much about.  If in doubt, try
brute force.  I figured out that the infinite regions would be those
intersecting with the boundary of a large enough rectangle, but I used a
rectangle 9 times the size of the smallest bounding rectangle, to be safe.
That worked, but was clearly overkill.  (Later, I learned from
[Jo Wood's notes](https://github.com/jwoLondon/adventOfCode/blob/master/literateElm/d06_2018.md)
that the smallest bounding rectangle was enough.)  For the finite regions,
I used the `frequency` utility function again.

The second part, total distances for each point, was easier.  I made the
search area a bit bigger than the smallest bounding rectangle, which
would be necessary if all the points were close together, but probably
wasn't needed for the supplied inputs.

## Day 7: The Sum of Its Parts

The first part was a topological sort, but was easy enough to craft
by hand.  I decided to stop if there were no runnable tasks, so it
would fail cleanly if any dependencies were cyclic.  The second part,
scheduling with multiple servers, was significantly more challenging.

With an imperative problem like this, one approach is to define a
recursive function of type `State -> Result`, transforming the state for
a tail-recursive call, but I prefer to define the state transformation
`State -> State`.  Then I can check it by examining the list of states
generated using `iterate`.

I started to write that, but was soon tied in knots about which version
of each value to use at each point.  So I decided to break it up into
three separate state transformations:

* starting new jobs on idle servers
* advancing the clock
* wrapping up any completed jobs

Each of these was manageable by itself.  Also, it was clear that
the termination test should be before advancing the clock, and this
breakdown made it easy to unroll the loop.  For the advancing step,
it would suffice to move the clock one tick at a time, but it's just as
easy to advance to the time when the next job will complete.

## Day 8: Memory Maneuver

This was familiar territory, and clearly suggested a recursive tree type.
The main task was to build the tree by scanning the list of numbers.
Once that was done, both parts were simple folds of the tree.

Lazy evaluation came in handy in the second part: one could define a list
of recursively valued child trees, and then repeated references to the same
child would only compute it once, and unused children would not be computed.

## Day 9: Marble Mania

This was straightforward, though fiddly in the details.  Because all
the changes occur near the current position in the circle, a simple
approach to was to break the circle as a deque with the current element
at one end, and that turned out to be enough.  (I used the Haskell
[Seq](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html)
container.)  The rest was following detailed instructions accurately.

The second part was the same with a larger input.  (I was expecting this,
as it is a recurring Advent of Code motif, but it caught out people who
used less flexible sequence types.)  The deque implementation ran out
of stack in the interpreter, but compiling it avoided that and was fast
enough for this input.

## Day 10: The Stars Align

This was fun.  Simulating the motion of the points was easy enough,
but the spare specification of the end condition made it interesting.
We had to work out which part of the space to draw, and when it might
contain a message.

## Day 11: Chronal Charge

The first part was simple list comprehensions, but using this naive approach
on the generalization in the second part would be O(*n*<sup>5</sup>),
which I assumed would be too slow.  At first it was quite daunting, but
then it became clear that a dynamic programming approach could compute
all the sums in O(*n*<sup>3</sup>) time.  It also turned out to be a
neat application of bulk operations on grids of numbers.  Again I had
to compile the solution because it ran out of stack in the interpreter.

Going through the subreddit later, I learned that one can easily compute
all the sums in O(*n*<sup>3</sup>) time using a
[summed-area table](https://en.wikipedia.org/wiki/Summed-area_table),
so people familiar with that structure would have found the second part
quite easy.

Some people had just used the naive O(*n*<sup>5</sup>) algorithm and got
answers in time.  This worked for this formulation because values range
uniformly between -5 and 4, and thus have an average of -0.5, so the
sums of large squares tend to be negative and one need only consider
the smaller ones.  (This is the discrete version of an
[Irwin–Hall distribution](https://en.wikipedia.org/wiki/Irwin%E2%80%93Hall_distribution),
so the sum of a square of size *k*<sup>2</sup> has expected value
-*k*<sup>2</sup>/2 with standard deviation approximately 5*k*/√3.)

This problem is a 2-dimensional
[maximum subarray problem](https://en.wikipedia.org/wiki/Maximum_subarray_problem),
and perhaps there are algorithms that do not require computing all the sums
(like Kadane's for the 1-dimensional case).

## Day 12: Subterranean Sustainability

One of those where the first part is a straightforward implementation
and the second part asks for the same thing but with many more steps.
In this case, we are asked to implement a 1-dimensional
[cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton).
The general case appears intractable, so I tried examining the states
generated by the supplied input, and the pattern soon became clear.

## Day 13: Mine Cart Madness

The description is complicated, and takes quite a while to process.
All the tracks seem to be rectangular, but that turns out not to matter.
After all this, there isn't much choice for the representation, and
the processing the input and implementing the rules is lengthy, but
mostly straightforward.  The top level can be simplified with a couple
of general higher-order functions, one of which I had to write.

The second part is mostly a simple adaptation of the first, but with an
added complication: one has to determine the order of the carts at the
start of each step, but by the time we reach a cart, it might already
have been destroyed in a crash.  As seen before in AoC, this does not
arise in the example given, but does happen with the supplied input.

## Day 14: Chocolate Charts

My original solutions to both parts involved iteratively updating the
state, with fairly messy termination conditions.  I used the Haskell
[Seq](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html)
container again, so that none of the operations were too costly, but an
imperative extensible array would also have worked.

A much neater solution is to generate the infinite list of scores,
from which both parts can be easily computed.

## Day 15: Beverage Bandits

This was nasty.  There were some similarities to Day 13, but this was
much more intricate.  The shortest-path calculation can be done cleanly
with a breadth-first search (used for several of the 2016 problems),
but the detailed rules took a lot of implementing.  The need to detect
an incomplete round required additional contortions.

My solution worked with all the examples, but its answer on the full
input was rejected.  Since there's nothing to indicate what went wrong,
there was no choice but to re-examine all the logic.  Still stuck,
I found the issue in a Reddit posting
["Details easy to be wrong on"](https://www.reddit.com/r/adventofcode/comments/a6f100/day_15_details_easy_to_be_wrong_on/).
As is often the case in AoC, it didn't arise with the supplied examples.
It was easily fixed, at the cost of even messier code.

The second part was an easy extension.

## Day 16: Chronal Classification

This was a neat inversion of the assembler puzzles of previous years,
though the input processing is quite messy.

The first part is straightforward, and suggests a strategy that works for
the more open second part, at least for this input.  Other inputs might
also have required the dual strategy.

## Day 17: Reservoir Research

This is a very novel problem.  The statement is noncommittal about the
order in which water flows sideways, which guides one towards computing
the full horizontal extent of the flow at once.  I then went for a
simplistic implementation, spreading water at each step until the set
of wet tiles converged.  This was fairly easy to get right, and worked
on the test input.  It seemed to be running forever on the actual input,
but stopped after 5 minutes with the correct answer.  It could be made
much faster by focussing on the bits that are growing, but getting that
right in a hurry is difficult.

The second part is an easy variation.

## Day 18: Settlers of The North Pole

Another
[cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton),
this time 2-dimensional.  The first part is straightforward, to check
that the rules are correctly implemented.

The second part is scaled up so much that the same approach will obviously
take far too long.  It might be feasible if the states repeat early and
often enough, and plugging in the repeating list abstraction I wrote
after thinking about the Day 12 puzzle did the trick.  (On my input,
the states repeat every 28 steps after an initial 480.)

## Day 19: Go With The Flow

A regular appearance on AoC: the first part is implementing a simple
machine and running the input code on it, while the second part requires
decompiling the code to create a more efficient equivalent.  The machine
here uses eight of the instructions from Day 16, though with a twist:
one of the registers holds the instruction pointer.

## Day 20: A Regular Map

This problem requires three steps, which are clearly laid out in the
description:

1. parsing the "regular expressions" (actually just concatenation and
   alternatives),
2. converting the expression into a maze, and
3. finding shortest paths in the maze.

The challenge is in the second step, mainly in choosing a representation
for the maze and deciding how to generate it.  A neat feature (also seen
previously on Days 10 and 12) is that the bounds of the area of interest
are not clear in advance, making it difficult to use fixed array structures.

The third step is easy given a breadth-first searcher, and the second
part is also easily derived from the breadth-first search.

## Day 21: Chronal Conversion

Another problem using the same assembly code as Days 16 and 19.  It turns
out not to be necessary to decompile the whole thing, just the outer loop,
and then simulate execution monitoring the termination test of that loop.

It took a little while to realize that the second part is a return to the
theme of detecting a cycle in the iterations of a function, as seen on
Days 12 and 18.  Simulating execution to do that took 4 minutes; one would
have to decompile and optimize the assembly program to make it faster.

The problem description is padded with an irrelevant discussion of the
initial part of the program, which has no effect.  It also seems that
the "lowest" qualification in both parts is redundant, but included to
obscure the uniqueness of the solution.

## Day 22: Mode Maze

The first part is building the maze and testing that it is correct (though
neither the example nor the question test the area beyond the target).
Since we don't know how much of it we will use, the proper approach would
be a memoized function over the infinite quarter-plane, but I just used
a large enough finite area.  I took a ridiculous amount of time getting
the details of the cell calculations right.

The second part was a path search, for which I used the breadth-first
searcher I had to hand.  This approach makes a lot of extra states
while one is in the process of switching equipment, but it was enough.
A proper approach would have turned the maze into a weighted graph with
admissible position-equipment pairs as nodes, and then used a general
path-finding algorithm.
