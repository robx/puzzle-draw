.PHONY: all examples clean

PUZZLES = $(wildcard *.pzl)
PNGTARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))
PDFTARGETS = $(patsubst %.pzl,%.pdf,$(PUZZLES))

all: pngs

pngs: $(PNGTARGETS)

pdfs: $(PDFTARGETS)

clean:
	rm -f $(PNGTARGETS) $(PDFTARGETS)

solution-%.png: solution-%.pzl
	stack exec drawpuzzle -- -c -e -f png $<

%.png: %.pzl
	stack exec drawpuzzle -- -e -f png $<

%.pdf: %.pzl
	stack exec drawpuzzle -- -e -f pdf $<
