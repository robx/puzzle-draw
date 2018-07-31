.PHONY: all puzzles examples clean compare

DRAW = drawpuzzle

EXAMPLES = $(wildcard *-example.pzl)
PUZZLES = $(filter-out $(EXAMPLES), $(wildcard *.pzl))

EXAMPLETARGETS = $(patsubst %.pzl,%.png,$(EXAMPLES))
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))
PUZZLEPDFTARGETS = $(patsubst %.pzl,%.pdf,$(PUZZLES))

all: puzzles examples

puzzles: $(PUZZLETARGETS)

puzzles-pdf: $(PUZZLEPDFTARGETS)

examples: $(EXAMPLETARGETS)

clean:
	rm -f $(PUZZLETARGETS) $(EXAMPLETARGETS)

$(EXAMPLETARGETS): %.png: %.pzl
	$(DRAW) -e -c -f png $<

$(PUZZLETARGETS): %.png: %.pzl
	$(DRAW) -p -f png $<

$(PUZZLEPDFTARGETS): %.pdf: %.pzl
	$(DRAW) -p -f pdf $<

compare:
	rm -f cur/*.png diff/*.png
	mkdir -p cur diff
	for f in $(EXAMPLES); do \
		$(DRAW) -d cur -e -c -f png $$f ; \
	done
	for f in $(PUZZLES); do \
		$(DRAW) -d cur -p -f png $$f ; \
	done
	for f in *.png; do \
		diff $$f cur/$$f > /dev/null || \
			(touch diff/$$f ; compare $$f cur/$$f diff/$$f) || \
			true ; \
	done
	./gallery.sh diff/*.png > gallery.html
	! ls diff/*.png
