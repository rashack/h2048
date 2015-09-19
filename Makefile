SRC := $(shell find . -name \*.hs -not -path ./dist/\* -not -name Setup.hs | xargs)

tags:
	hasktags -e $(SRC)

check:
	hlint "--ignore=Eta reduce" $(SRC)

.PHONY: tags check

