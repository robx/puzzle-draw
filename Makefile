.PHONY: test

test:
	stack test
	stack exec test-compare
