SRC := $(wildcard src/*.hs)

tags:
	hasktags -e $(SRC)

check:
	hlint "--ignore=Eta reduce" $(SRC)

.PHONY: tags check
