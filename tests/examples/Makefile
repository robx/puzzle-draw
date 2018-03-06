.PHONY: all examples clean

PUZZLES = $(wildcard *.pzl)
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))

all: examples

examples: $(PUZZLETARGETS)

clean:
	rm -f $(PUZZLETARGETS)

solution-%.png: solution-%.pzl
	stack exec drawpuzzle -- -c -e -f png $<

%.png: %.pzl
	stack exec drawpuzzle -- -e -f png $<
