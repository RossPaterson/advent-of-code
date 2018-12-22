# Notes on each day's solutions
  
![Completion times 2016](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2016.png)

(image by Jo Wood)

## Day 1: No Time for a Taxicab

The first part is just following Cartesian directions.

The second part was a significant variation.

## Day 2: Bathroom Security

The first part is following directions on a keypad (easier).

The second part was a bit harder.

## Day 3: Squares With Three Sides

Checking for triangle sides is a simple list manipulation.

The second part needs `transpose` and `takes`.

## Day 4: Security Through Obscurity

Finding most common letters, second part shift cipher.

## Day 5: How About a Nice Game of Chess?

The first part was a simple list search to invert an MD5 hash.

The second part builds an inverted array.

## Day 6: Signals and Noise

Most and least common letters.

## Day 7: Internet Protocol Version 7

Substring searches.

The second part is incompletely specified.
It is not clear that "corresponding" means exactly one.

## Day 8: Two-Factor Authentication

Rotations of rows and columns of a grid.

The second part involves displaying a bitmap for manual reading.

## Day 9: Explosives in Cyberspace

String expansion.

The second part is incompletely specified; needs recursion.
It is crucial that markers must be nested.

## Day 10: Balance Bots

An asynchonous network, incompletely specified.

## Day 11: Radioisotope Thermoelectric Generators

After a run of medium-sized problems, this one came as a shock.
It is a 3-stage variant of the
[Jealous Husbands Problem](https://en.wikipedia.org/wiki/Missionaries_and_cannibals_problem).
The first part is to get the implementation correct,
but simple methods take far too long for the second part.
I did manage to get an answer by using the
[A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm)
and a hand-optimized representation, but none of that was necessary,
or as fast as exploiting the structure of the problem.

**Lesson:** merge equivalent states to cut the search space.

## Day 12: Leonardo's Monorail

A machine simulation.

## Day 13: A Maze of Twisty Little Cubicles

A cute maze solver using breadth-first search.

## Day 14: One-Time Pad

Lots of MD5 calls, but otherwise simple.

## Day 15: Timing is Everything

Easily recognizable as the
[Chinese Remainder Problem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem).

## Day 16: Dragon Checksum

Fractal strings of bits.

## Day 17: Two Steps Forward

Cute dynamic maze calling MD5.

## Day 18: Like a Rogue

An easy iteration.

## Day 19: An Elephant Named Joseph

Two variations on the
[Josephus problem](https://en.wikipedia.org/wiki/Josephus_problem).

There are fast shortcuts, but brute force with the Haskell
[Seq](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html)
container worked when array-based implementations were prohibitively slow.

## Day 20: Firewall Rules

Merging intervals.

## Day 21: Scrambled Letters and Hash

The second part is the inverse of the first.
The inverse is not unique on the example, but is on puzzle input.

## Day 22: Grid Computing

The second part as specified is very difficult, but the given input is a
much simpler case reducing to a 15-puzzle.

## Day 23: Safe Cracking

Self-modifying code (based of Day 12).

## Day 24: Air Duct Spelunking

A maze search (Day 13) plus the
[Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem).

## Day 25: Clock Signal

Decompiling of a similar assembly language to Days 12 and 23.
