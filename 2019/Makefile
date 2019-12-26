LIBDIR := ../utilities
PROGS := $(sort $(patsubst %.hs,%,$(wildcard Day*.hs)))

all: $(PROGS)

run: $(PROGS)
	for prog in $(PROGS); do echo $$prog:; ./$$prog; echo; done

clean:
	$(RM) $(PROGS) *.o *.hi $(LIBDIR)/*.o $(LIBDIR)/*.hi

Day%: Day%.hs $(LIBDIR)/*.hs
	/usr/bin/ghc -Wall --make -O -i$(LIBDIR) Day$*.hs
	@touch $@

Day02: Intcode.hs
Day05: Intcode.hs
Day07: Intcode.hs
Day09: Intcode.hs
Day11: Intcode.hs
Day13: Intcode.hs
Day13orig: Intcode.hs
Day15: Intcode.hs
Day15orig: Intcode.hs
Day17: Intcode.hs
Day19: Intcode.hs
Day21: Intcode.hs
Day23: Intcode.hs
Day25: Intcode.hs
