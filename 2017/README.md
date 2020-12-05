# Notes on each day's solutions

![Completion times 2017](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2017.png)

(image by [Jo Wood](https://github.com/jwoLondon))

Overall, this year was the comparatively easy.

## [Day 1: Inverse Captcha](https://adventofcode.com/2017/day/1)

A simple list manipulation easily done with zips.

## [Day 2: Corruption Checksum](https://adventofcode.com/2017/day/2)

Another easy list comprehension exercise.

## [Day 3: Spiral Memory](https://adventofcode.com/2017/day/3)

Converting between a square spiral and cartesian coordinates, a bit more
involved than surrounding puzzles.

## [Day 4: High-Entropy Passphrases](https://adventofcode.com/2017/day/4)

Counting strings satisfying certain constraints.

## [Day 5: A Maze of Twisty Trampolines, All Alike](https://adventofcode.com/2017/day/5)

This was a straightforward implementation of a sequential process.

## [Day 6: Memory Reallocation](https://adventofcode.com/2017/day/6)

Detecting a cycle in an iteration.

## [Day 7: Recursive Circus](https://adventofcode.com/2017/day/7)

The first part sets up a multiway tree.  The second is more involved,
combining upward and downward traversals.

## [Day 8: I Heard You Like Registers](https://adventofcode.com/2017/day/8)

This puzzle involves executing a linear assembly program.  The first
part asks about the final state, while the second queries all states
in the sequence.

## [Day 9: Stream Processing](https://adventofcode.com/2017/day/9)

This is a parsing problem with nested brackets and leaf strings with
character escapes.  After generating the parse tree, both parts are
easily computed.

## [Day 10: Knot Hash](https://adventofcode.com/2017/day/10)

This task introduces a costly, non-invertible function, defined as a
tedious series of simple steps.  This function is re-used on day 14.

## [Day 11: Hex Ed](https://adventofcode.com/2017/day/11)

This task is based on hexagonal grids, rather than the familiar
rectangular sort.  They crop up in games and visualization, and
several convenient coordinate systems have been devised (e.g. see
[this blog](https://www.redblobgames.com/grids/hexagons/)).

The first part asks about the final state of a sequence, while the
second queries all the states.

## [Day 12: Digital Plumber](https://adventofcode.com/2017/day/12)

This task deals with an undirected graph.  The first part asks for one
connected component.  The second asks for the number of connected components.

## [Day 13: Packet Scanners](https://adventofcode.com/2017/day/13)

It takes a while to decode this one, before realizing that it's a
something of a reverse Chinese Remainder Problem.  Considerable care is
needed to avoid off-by-one errors.  There might be more sophisticated
approaches to the second part, but exhaustive search is feasible.

## [Day 14: Disk Defragmentation](https://adventofcode.com/2017/day/14)

The first part involves generation of a pseudorandom 2-dimensional bitmap
using a cumbersome combination of the Knothash function from day 10 and
some bit manipulation.  The second asks for the number of connected
components.

## [Day 15: Dueling Generators](https://adventofcode.com/2017/day/15)

This was an easy list exercise.  The first part is a zip of two iterations.
In the second part, each of the iterations is filtered before zipping.

## [Day 16: Permutation Promenade](https://adventofcode.com/2017/day/16)

This was fun to figure out.  For the second part, we need a fast way of
repeating a move sequence a lot of times.  It is apparent that a state
is a permutation, but the key insight is a representation of moves as
a monoid, so that we can use the log-time repetition algorithm.

## [Day 17: Spinlock](https://adventofcode.com/2017/day/17)

This is a [Josephus
problem](https://en.wikipedia.org/wiki/Josephus_problem) run in reverse.
The first part is easy with the `Seq` container.  The second involves
many more steps, but to select insertions after the zero, we don't need
the keep track of the whole sequence, just the current position.

## [Day 18: Duet](https://adventofcode.com/2017/day/18)

The first part is execution of a simple assembly language.  The second
involves two such machines connected by message queues.  The details
are fiddly, but not difficult.

## [Day 19: A Series of Tubes](https://adventofcode.com/2017/day/19)

The main challenge here is tracing out a path from an ASCII drawing.
After that, both parts are straightforward.

## [Day 20: Particle Swarm](https://adventofcode.com/2017/day/20)

A puzzle involving a particle system, which needs to be solved
analytically rather than by stepwise simulation.  The first part asks for
asymptotic behaviour.  The second part is a different question involving
collisions, but with the additional complication that an intersection
of particle trajectories is not a collision if one of the particles has
been destroyed at an earlier time.  Getting this right involves careful
manipulation of intermediate values.

## [Day 21: Fractal Art](https://adventofcode.com/2017/day/18)

This involves manipulations of square grids.  It is somewhat odd that
the lefthand side of a rewrite rule can be flipped and rotated with no
effect on the righthand side.  The second part asks for more steps, but
the same method works for that too.

## [Day 22: Sporifica Virus](https://adventofcode.com/2017/day/22)

This amounts to a Turing machine with a 2-dimensional "tape".  There are
details to take care of, but implementation is not difficult.  In the
second part the alphabet is increased from 2 symbols to 4, with actions
for the new symbols, which requires a minor modification from the
first part.

## [Day 23: Coprocessor Conflagration](https://adventofcode.com/2017/day/23)

This involves another assembly language.  The first part checks the
implementation, and is straightforward.  The challenge is in the second
part, which involves disassembling the program to work out what it does,
and then implementing that more efficiently.

## [Day 24: Electromagnetic Moat](https://adventofcode.com/2017/day/24)

This puzzle involves taking a list of dominoes finding the "best" string
out of them, with different criteria for "best" in each part.  Exhaustive
search, with no special data structures, is sufficient for both parts.

## [Day 25: The Halting Problem](https://adventofcode.com/2017/day/25)

The puzzle asks for an actual Turing machine implementation.  This is
straightforward, except for a bit of care with the infinite tape.
