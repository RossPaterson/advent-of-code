YEARS := $(sort $(wildcard 20??))

all: $(YEARS)

run: $(YEARS)

test: $(YEARS)

clean: $(YEARS)

$(YEARS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: all run test clean $(YEARS)
