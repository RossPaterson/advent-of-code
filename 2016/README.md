# Notes on each day's solutions
  
![Completion times 2016](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2016.png)

(image by Jo Wood)

Days 5, 14 and 17 used MD5 hashing (used in Day 5 in 2015), exploiting
the fact that it is a bit costly to compute and is difficult to invert.
In 2017, a custom hashing function with the same properties is used
instead.

## [Day 1: No Time for a Taxicab](https://adventofcode.com/2016/day/1)

The first part is just following Cartesian directions.

The second part was a significant variation.

## [Day 2: Bathroom Security](https://adventofcode.com/2016/day/2)

The first part is following directions on a keypad (easier).

The second part was a bit harder.

## [Day 3: Squares With Three Sides](https://adventofcode.com/2016/day/3)

Checking for triangle sides is a simple list manipulation.

The second part needs `transpose` and `takes`.

## [Day 4: Security Through Obscurity](https://adventofcode.com/2016/day/4)

Finding most common letters, second part shift cipher.

## [Day 5: How About a Nice Game of Chess?](https://adventofcode.com/2016/day/5)

The first part was a simple list search to invert an MD5 hash.

The second part builds an inverted array.

## [Day 6: Signals and Noise](https://adventofcode.com/2016/day/6)

Most and least common letters.

## [Day 7: Internet Protocol Version 7](https://adventofcode.com/2016/day/7)

Substring searches.

The second part is incompletely specified.
It is not clear that "corresponding" means exactly one.

## [Day 8: Two-Factor Authentication](https://adventofcode.com/2016/day/8)

Rotations of rows and columns of a grid.

The second part involves displaying a bitmap for manual reading.

## [Day 9: Explosives in Cyberspace](https://adventofcode.com/2016/day/9)

String expansion.

The second part is incompletely specified: it is crucial that markers
must be nested.  The solution requires recursion, but fortunately only
lengths need be computed.

## [Day 10: Balance Bots](https://adventofcode.com/2016/day/10)

An asynchonous network, incompletely specified.

## [Day 11: Radioisotope Thermoelectric Generators](https://adventofcode.com/2016/day/11)

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

## [Day 12: Leonardo's Monorail](https://adventofcode.com/2016/day/12)

A machine simulation.

## [Day 13: A Maze of Twisty Little Cubicles](https://adventofcode.com/2016/day/13)

A cute maze solver using breadth-first search.

## [Day 14: One-Time Pad](https://adventofcode.com/2016/day/14)

Lots of MD5 calls, but otherwise simple.

## [Day 15: Timing is Everything](https://adventofcode.com/2016/day/15)

Easily recognizable as the
[Chinese Remainder Problem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem).

## [Day 16: Dragon Checksum](https://adventofcode.com/2016/day/16)

Fractal strings of bits.

## [Day 17: Two Steps Forward](https://adventofcode.com/2016/day/17)

Cute dynamic maze calling MD5.

## [Day 18: Like a Rogue](https://adventofcode.com/2016/day/18)

An easy iteration.

## [Day 19: An Elephant Named Joseph](https://adventofcode.com/2016/day/19)

Two variations on the
[Josephus problem](https://en.wikipedia.org/wiki/Josephus_problem).

There are fast shortcuts, but brute force with the Haskell
[Seq](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html)
container worked when array-based implementations were prohibitively slow.

## [Day 20: Firewall Rules](https://adventofcode.com/2016/day/20)

Merging intervals.

## [Day 21: Scrambled Letters and Hash](https://adventofcode.com/2016/day/21)

The second part is the inverse of the first.
The inverse is not unique on the example, but is on puzzle input.

## [Day 22: Grid Computing](https://adventofcode.com/2016/day/22)

The second part as specified is very difficult, but the given input is a
much simpler case reducing to a 15-puzzle.

## [Day 23: Safe Cracking](https://adventofcode.com/2016/day/23)

Self-modifying code (based of Day 12).

## [Day 24: Air Duct Spelunking](https://adventofcode.com/2016/day/24)

A maze search (Day 13) plus the
[Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem).

## [Day 25: Clock Signal](https://adventofcode.com/2016/day/25)

Decompiling of a similar assembly language to Days 12 and 23.
