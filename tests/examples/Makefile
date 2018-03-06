.PHONY: all examples clean

PUZZLES = $(wildcard *.pzl)
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))

all: examples

examples: $(PUZZLETARGETS)

clean:
	rm -f $(PUZZLETARGETS)

solution-%.png: solution-%.pzl
	drawpuzzle -c -e -f png $<

%.png: %.pzl
	drawpuzzle -e -f png $<
