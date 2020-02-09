.PHONY: format test compare ghcid

LOCALINSTALL := $(shell stack $(STACKARGS) path | grep ^local-install-root: | awk '{print $$2}')/bin
DRAW = $(LOCALINSTALL)/drawpuzzle

ghcid:
	ghcid

test:
	stack test

compare:
	$(MAKE) -C tests/examples compare DRAW=$(DRAW)

format:
	find src -name '*.hs' | xargs ormolu -m inplace
	find tests -name '*.hs' | xargs ormolu -m inplace
