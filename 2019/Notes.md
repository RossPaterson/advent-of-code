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
