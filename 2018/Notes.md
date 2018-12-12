# Notes on each day's solutions

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
at one end, and that turned out to be enough.  The rest was following
detailed instructions accurately.

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
[summed-area table group](https://en.wikipedia.org/wiki/Summed-area_table),
so people familiar with that structure would find part 2 quite easy.
And some people had just used the naive O(*n*<sup>5</sup>) algorithm
and got answers in time.

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
