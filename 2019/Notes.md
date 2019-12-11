# Notes on each day's solutions

![Completion times 2019](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2019.png)

(image by Jo Wood)

The most enjoyable puzzles so far have been days 3, 6, 7 and 10.

Some common AoC themes:

* assembly code: days 2, 5, 7, 9, ... (used as a black box on day 11)

## [Day 1: The Tyranny of the Rocket Equation](https://adventofcode.com/2019/day/1)

Another simple one to start with.  The first part is a trivial calculation,
which is iterated in the second part.

## [Day 2: 1202 Program Alarm](https://adventofcode.com/2019/day/2)

Introducing this year's assembly language.  The first part is straightforward
implementation of the machine.  The second part is an inversion, for which
exhaustive search was sufficient.

On later examination, the code turns out to be relatively simple, not
modifying any instructions that are executed later.  The second and third
instructions have no effect, but are there to provide a range of constants
to be added or multiplied to the first input value, to which the second
input value is then added, so it computes A*x + y + B for some constants
A and B.

We are told the machine will be used later, with more instructions,
presumably including one to set the instruction pointer from a value in
memory.  It is a stored-program computer that executes self-modifying
programs with instructions of different lengths, so it's going to
be tricky.

## [Day 3: Crossed Wires](https://adventofcode.com/2019/day/3)

Generating the points visited by a wire is similar to previous puzzles.
The rest was a nice exercise in operating on whole Sets and Maps.

(I initially solved a more general problem, having somehow missed that
there were only two wires.  That was another nice exercise, using `Map`
and several applications of `unionsWith`.)

## [Day 4: Secure Container](https://adventofcode.com/2019/day/4)

This is a smallish enumeration exercise.  It can be handled with a
generate-and-test approach, making one of the criteria the generator
and the others independent tests.  The non-decreasing criterion made an
economical generator: even if you ignore the range and use all 10 digits,
it still only makes 5005 lists.  The second part involves replacing one
of the tests.  That would have been harder if it had originally been
integrated with the generator.

It turns out even that was too much optimization.  Simply using the
range as the generator would be fast enough.

## [Day 5: Sunny with a Chance of Asteroids](https://adventofcode.com/2019/day/5)

The task is to fill out the instruction set of the computer with I/O
and addressing modes (part 1) and jumps and tests (part 2).  It's all
straightforward if tedious following of a detailed description.
The computer now seems complete, presumably for more use later.

## [Day 6: Universal Orbit Map](https://adventofcode.com/2019/day/6)

This was a nice little exercise involving a bottom-up view of trees.
I used memoization for calculating the depth of each node in the first
part because it's so easy in a lazy language, but a naive quadratic
recursion would have been fast enough.

The second part can be cleanly decomposed into generating paths to the
root and using those to count transfers.  The paths require a similar
accumulation to the first part, so I used the same structure again,
even though we only need two of them.

## [Day 7: Amplification Circuit](https://adventofcode.com/2019/day/7)

This was fun.  Using the Intcode program as a black box, the first part
is a straightforward left fold over a list of instances with different
phases.

The second part requires placing a slight modification of that fold
inside a feedback loop, which is a big win for a lazy language.  Then the
Intcode computer needed to be updated to produce its outputs lazily.

## [Day 8: Space Image Format](https://adventofcode.com/2019/day/8)

There's a lot to read here, but after all that the problem is easy list
manipulation, using `transpose` twice in the second part.

## [Day 9: Sensor Boost](https://adventofcode.com/2019/day/9)

The task is to add relative addressing to the Intcode computer and to
explicitly remove a couple of restrictions that we may have imposed.
This is fairly straightforward, and the supplied input program does some
useful testing of the implementation.  The resulting computer also solves
part 2 with no extra work.  We are told that the computer is now complete.

Now that the computer has relative addressing, it can express recursion.
For example, in part 2 the program implements f(A) + B for constants
A and B and the function `f(n) = if n < 3 then n else f(n-1) + f(n-3)`
([OEIS A097333](https://oeis.org/A097333)).

## [Day 10: Monitoring Station](https://adventofcode.com/2019/day/10)

This is an ingenious problem, requiring careful choice of representations
and multi-stage use of sets and maps.  The first part sets up the scenario,
while the second takes it further.

In the second part, I constructed a representation of angles in discrete
2D space that gets them in the right order.  `atan2` on differences would
be prone to floating point precision problems, which could be fixed by
cancelling common factors first, but it seems more in the spirit of the
puzzle to keep everything discrete.

## [Day 11: Space Police](https://adventofcode.com/2019/day/11)

A moderate problem combining some previous themes: moving around on a
2D grid (day 3), running a black box Intcode program in a feedback loop
(day 7), and displaying a bitmap image (day 8).
