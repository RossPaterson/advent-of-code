# Common rules for Makefiles in year directories
# The including Makefile should set the following variables:
#
#	YEAR - matching the directory name
#	EXTRA_LIBS - extra -l options for the compiler

LIBDIR := ../utilities
UTILITIES := $(wildcard $(LIBDIR)/*.hs) $(wildcard $(LIBDIR)/*/*.hs)
PROGS := $(sort $(patsubst %.hs,%,$(wildcard Day??.hs)))
OUTPUTS := $(sort $(patsubst output/%.txt,%,$(wildcard output/??.txt)))
COMPILER := /usr/bin/ghc -Wall --make -O -i$(LIBDIR)

.DELETE_ON_ERROR:

all: $(PROGS)

run: $(PROGS)
	for prog in $(PROGS); do echo $$prog:; ./$$prog; echo; done

test: $(PROGS)
	for n in $(OUTPUTS); do echo Day$$n; if ./Day$$n | diff output/$$n.txt -; then :; else exit 1; fi; done

clean:
	$(RM) $(PROGS) *.o *.hi
	$(RM) $(LIBDIR)/*.o $(LIBDIR)/*.hi $(LIBDIR)/*/*.o $(LIBDIR)/*/*.hi

Day%: Day%.hs $(UTILITIES)
	$(COMPILER) Day$*.hs $(EXTRA_LIBS)
	@touch $@

Day01: Day01.hs $(UTILITIES) input/01.txt
	$(COMPILER) Day01.hs $(EXTRA_LIBS)
	@touch Day01

Day02: Day02.hs $(UTILITIES) input/02.txt
	$(COMPILER) Day02.hs $(EXTRA_LIBS)
	@touch Day02

Day03: Day03.hs $(UTILITIES) input/03.txt
	$(COMPILER) Day03.hs $(EXTRA_LIBS)
	@touch Day03

Day04: Day04.hs $(UTILITIES) input/04.txt
	$(COMPILER) Day04.hs $(EXTRA_LIBS)
	@touch Day04

Day05: Day05.hs $(UTILITIES) input/05.txt
	$(COMPILER) Day05.hs $(EXTRA_LIBS)
	@touch Day05

Day06: Day06.hs $(UTILITIES) input/06.txt
	$(COMPILER) Day06.hs $(EXTRA_LIBS)
	@touch Day06

Day07: Day07.hs $(UTILITIES) input/07.txt
	$(COMPILER) Day07.hs $(EXTRA_LIBS)
	@touch Day07

Day08: Day08.hs $(UTILITIES) input/08.txt
	$(COMPILER) Day08.hs $(EXTRA_LIBS)
	@touch Day08

Day09: Day09.hs $(UTILITIES) input/09.txt
	$(COMPILER) Day09.hs $(EXTRA_LIBS)
	@touch Day09

Day10: Day10.hs $(UTILITIES) input/10.txt
	$(COMPILER) Day10.hs $(EXTRA_LIBS)
	@touch Day10

Day11: Day11.hs $(UTILITIES) input/11.txt
	$(COMPILER) Day11.hs $(EXTRA_LIBS)
	@touch Day11

Day12: Day12.hs $(UTILITIES) input/12.txt
	$(COMPILER) Day12.hs $(EXTRA_LIBS)
	@touch Day12

Day13: Day13.hs $(UTILITIES) input/13.txt
	$(COMPILER) Day13.hs $(EXTRA_LIBS)
	@touch Day13

Day14: Day14.hs $(UTILITIES) input/14.txt
	$(COMPILER) Day14.hs $(EXTRA_LIBS)
	@touch Day14

Day15: Day15.hs $(UTILITIES) input/15.txt
	$(COMPILER) Day15.hs $(EXTRA_LIBS)
	@touch Day15

Day16: Day16.hs $(UTILITIES) input/16.txt
	$(COMPILER) Day16.hs $(EXTRA_LIBS)
	@touch Day16

Day17: Day17.hs $(UTILITIES) input/17.txt
	$(COMPILER) Day17.hs $(EXTRA_LIBS)
	@touch Day17

Day18: Day18.hs $(UTILITIES) input/18.txt
	$(COMPILER) Day18.hs $(EXTRA_LIBS)
	@touch Day18

Day19: Day19.hs $(UTILITIES) input/19.txt
	$(COMPILER) Day19.hs $(EXTRA_LIBS)
	@touch Day19

Day20: Day20.hs $(UTILITIES) input/20.txt
	$(COMPILER) Day20.hs $(EXTRA_LIBS)
	@touch Day20

Day21: Day21.hs $(UTILITIES) input/21.txt
	$(COMPILER) Day21.hs $(EXTRA_LIBS)
	@touch Day21

Day22: Day22.hs $(UTILITIES) input/22.txt
	$(COMPILER) Day22.hs $(EXTRA_LIBS)
	@touch Day22

Day23: Day23.hs $(UTILITIES) input/23.txt
	$(COMPILER) Day23.hs $(EXTRA_LIBS)
	@touch Day23

Day24: Day24.hs $(UTILITIES) input/24.txt
	$(COMPILER) Day24.hs $(EXTRA_LIBS)
	@touch Day24

Day25: Day25.hs $(UTILITIES) input/25.txt
	$(COMPILER) Day25.hs $(EXTRA_LIBS)
	@touch Day25

input/%.txt:
	../scripts/getinput $(YEAR) $* >$@
