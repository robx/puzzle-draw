.PHONY: format test compare ghcid

DRAW ?= $(shell cabal list-bin drawpuzzle)

ghcid:
	ghcid

test:
	stack test

compare:
	$(MAKE) -C tests/examples compare DRAW=$(DRAW)

format:
	find src -name '*.hs' | xargs ormolu -m inplace
	find tests -name '*.hs' | xargs ormolu -m inplace
