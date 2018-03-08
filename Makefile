.PHONY: test compare

test:
	stack test

compare:
	mkdir -p compare/diff
	cd compare && for file in *.png; do \
		compare $$file ../tests/examples/$$file diff/$$file || true ; \
	done
	sh tests/gallery.sh compare/*.png > compare/gallery.html
