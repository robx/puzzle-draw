.PHONY: all examples clean

PUZZLES = $(wildcard *.pzl)
PNGTARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))
PDFTARGETS = $(patsubst %.pzl,%.pdf,$(PUZZLES))

all: pngs

pngs: $(PNGTARGETS)

pdfs: $(PDFTARGETS)

clean:
	rm -f $(PNGTARGETS) $(PDFTARGETS)

%.png: %.pzl
	stack exec drawpuzzle -- -e -c -f png $<

%.pdf: %.pzl
	stack exec drawpuzzle -- -e -c -f pdf $<
