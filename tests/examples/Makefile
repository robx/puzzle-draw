.PHONY: all puzzles examples clean compare

EXAMPLES = $(wildcard *-example.pzl)
PUZZLES = $(filter-out $(EXAMPLES), $(wildcard *.pzl))

EXAMPLETARGETS = $(patsubst %.pzl,%.png,$(EXAMPLES))
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))

all: puzzles examples

puzzles: $(PUZZLETARGETS)

examples: $(EXAMPLETARGETS)

clean:
	rm -f $(PUZZLETARGETS) $(EXAMPLETARGETS)

$(EXAMPLETARGETS): %.png: %.pzl
	stack exec drawpuzzle -- -e -c -f png $<

$(PUZZLETARGETS): %.png: %.pzl
	stack exec drawpuzzle -- -p -f png $<

compare:
	rm -f cur/*.png diff/*.png
	mkdir -p cur diff
	for f in $(EXAMPLES); do \
		stack exec drawpuzzle -- -d compare -e -c -f png $$f ; \
	done
	for f in $(PUZZLES); do \
		stack exec drawpuzzle -- -d compare -p -f png $$f ; \
	done
	for f in *.png; do \
		diff $$f compare/$$f > /dev/null || compare $$f cur/$$f diff/$$f; \
	done
	sh gallery.sh > gallery.html
	! ls diff/*.png
