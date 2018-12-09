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
that the smallest bounding rectangle was enough.)

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

Each of these was manageable by itself.  Also, with this breakdown, it
was clear that the termination test should be before advancing the clock,
and this breakdown made it easy to unroll the loop.  For the advancing
step, it would suffice to move the clock one tick at a time, but it's
just as easy to advance to the time when the next job will complete.

## Day 8: Memory Maneuver

This was familiar territory, and clearly suggested a recursive tree type.
The main task was to build the tree by scanning the list of numbers.
Once that was done, both parts were simple folds of the tree.

Lazy evaluation came in handy in the second part: one could define a list
of recursively valued child trees, and then repeated references to the same
child would only compute it once, and unused children would not be computed.

## Day 9: Marble Mania

This was straightforward, though fiddly in the details.  Because all
the changes occur near the current position in the circle, the naive
approach to was to break the circle as a deque with the current element
at one end, and that turned out to be enough.  The rest was following
detailed instructions accurately.

The second part was the same with a larger input.  The naive
implementation ran out of stack in the interpreter, but compiling it
avoided that and was fast enough for this input.
