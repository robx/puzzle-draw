.PHONY: test compare

LOCALINSTALL := $(shell stack $(STACKARGS) path | grep ^local-install-root: | awk '{print $$2}')/bin
DRAW = $(LOCALINSTALL)/drawpuzzle

test:
	stack test

compare:
	$(MAKE) -C tests/examples compare DRAW=$(DRAW)
