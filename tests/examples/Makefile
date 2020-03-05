.PHONY: all puzzles examples graphics clean compare

DRAW = drawpuzzle

EXAMPLES = $(wildcard *-example.pzl)
EXAMPLESG = $(wildcard *-example.pzg)
PUZZLES = $(filter-out $(EXAMPLES), $(wildcard *.pzl))
PUZZLESG = $(filter-out $(EXAMPLESG), $(wildcard *.pzg))

EXAMPLETARGETS = $(patsubst %.pzl,%.png,$(EXAMPLES))
EXAMPLEGTARGETS = $(patsubst %.pzg,%.png,$(EXAMPLESG))
PUZZLETARGETS = $(patsubst %.pzl,%.png,$(PUZZLES))
PUZZLEGTARGETS = $(patsubst %.pzg,%.png,$(PUZZLESG))

all: puzzles examples graphics

puzzles: $(PUZZLETARGETS) $(PUZZLEGTARGETS)

examples: $(EXAMPLETARGETS) $(EXAMPLEGTARGETS)

clean:
	rm -f $(PUZZLETARGETS) $(EXAMPLETARGETS) $(PUZZLEGTARGETS) $(EXAMPLEGTARGETS)

compare:
	rm -f cur/*.png diff/*.png
	mkdir -p cur diff
	$(DRAW) -d cur -e -c -f png $(EXAMPLES) $(EXAMPLESG)
	$(DRAW) -d cur -p -c -f png $(PUZZLES) $(PUZZLESG)
	for f in *.png; do \
		diff $$f cur/$$f > /dev/null || \
			(touch diff/$$f ; compare $$f cur/$$f diff/$$f) || \
			true ; \
	done
	./gallery.sh diff/*.png > gallery.html
	! ls diff/*.png
