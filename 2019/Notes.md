# Notes on each day's solutions

![Completion times 2019](https://raw.githubusercontent.com/jwoLondon/adventOfCode/master/images/completionTimes2019.png)

(image by Jo Wood)

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
