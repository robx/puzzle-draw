.PHONY: all puzzles examples clean compare

DRAW = stack exec drawpuzzle --

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
	$(DRAW) -e -c -f png $<

$(PUZZLETARGETS): %.png: %.pzl
	$(DRAW) -p -f png $<

compare:
	rm -f cur/*.png diff/*.png
	mkdir -p cur diff
	for f in $(EXAMPLES); do \
		$(DRAW) -d compare -e -c -f png $$f ; \
	done
	for f in $(PUZZLES); do \
		$(DRAW) -d compare -p -f png $$f ; \
	done
	for f in *.png; do \
		diff $$f compare/$$f > /dev/null || compare $$f cur/$$f diff/$$f; \
	done
	sh gallery.sh > gallery.html
	! ls diff/*.png
