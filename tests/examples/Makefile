DRAW=../../dist/build/drawpuzzle/drawpuzzle

PUZZLES = $(wildcard *.pzl)
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))

all: examples

examples: $(PUZZLETARGETS)

clean:
	rm -f $(EXAMPLETARGETS)

%.png: %.pzl $(DRAW)
	$(DRAW) -e -f png $<
