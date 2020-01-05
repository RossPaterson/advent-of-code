LIBDIR := ../utilities
PROGS := $(sort $(patsubst %.hs,%,$(wildcard Day??.hs)))

all: $(PROGS)

run: $(PROGS)
	for prog in $(PROGS); do echo $$prog:; ./$$prog; echo; done

clean:
	$(RM) $(PROGS) *.o *.hi $(LIBDIR)/*.o $(LIBDIR)/*.hi

Day%: Day%.hs $(LIBDIR)/*.hs
	/usr/bin/ghc -Wall --make -O -i$(LIBDIR) Day$*.hs
	@touch $@
