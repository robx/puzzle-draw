.PHONY: test compare

test:
	stack test

compare:
	$(MAKE) -C tests/examples compare
