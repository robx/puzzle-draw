.PHONY: test

test:
	stack test
	stack exec test-compare

gallery:
	sh tests/gallery.sh *.png > gallery.html
