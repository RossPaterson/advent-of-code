LIBDIR := ../utilities
PROGS := $(sort $(patsubst %.hs,%,$(wildcard Day??.hs)))
OUTPUTS := $(sort $(patsubst output/%.txt,%,$(wildcard output/??.txt)))

all: $(PROGS)

run: $(PROGS)
	for prog in $(PROGS); do echo $$prog:; ./$$prog; echo; done

test: $(PROGS)
	for n in $(OUTPUTS); do echo Day$$n; if ./Day$$n | diff output/$$n.txt -; then :; else exit 1; fi; done

clean:
	$(RM) $(PROGS) *.o *.hi $(LIBDIR)/*.o $(LIBDIR)/*.hi

Day%: Day%.hs $(LIBDIR)/*.hs
	/usr/bin/ghc -Wall --make -O -i$(LIBDIR) Day$*.hs -lbsd
	@touch $@
