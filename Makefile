.PHONY: test test-compare compare

test:
	stack test

test-compare:
	mkdir -p compare
	cd compare && stack exec test-compare

compare:
	mkdir -p compare/diff
	cd compare && for file in *.png; do \
		compare $$file ../tests/examples/$$file diff/$$file || true ; \
	done
	sh tests/gallery.sh compare/*.png > compare/gallery.html
