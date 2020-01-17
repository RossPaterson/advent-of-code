YEARS := $(sort $(wildcard 20??))

all: $(YEARS)

clean: $(YEARS)

$(YEARS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: all clean $(YEARS)
