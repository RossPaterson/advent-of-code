LIBDIR := ../utilities
YEAR := 2019
SITE := https://adventofcode.com/$(YEAR)/day
MAINS := $(sort $(wildcard Day??.hs))
PROGS := $(patsubst %.hs,%,$(MAINS))
INPUTS := $(patsubst Day%.hs,input/%.txt,$(MAINS))

all: $(PROGS)

run: $(PROGS) inputs
	for prog in $(PROGS); do echo $$prog:; ./$$prog; echo; done

inputs: $(INPUTS)

clean:
	$(RM) $(PROGS) *.o *.hi $(LIBDIR)/*.o $(LIBDIR)/*.hi

Day%: Day%.hs $(LIBDIR)/*.hs
	/usr/bin/ghc --make -O -i$(LIBDIR) Day$*.hs

input/%.txt: ../session
	mkdir -p input
	curl -b session=`cat ../session` $(SITE)/`echo $* | sed 's/^0//'`/input >$@

../session:
	@echo "Put the session key in $@"; exit 1
