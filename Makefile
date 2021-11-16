STACK       = stack --allow-different-user

.PHONY: all test clean ghcid distclean

all: test

test:   clean
	$(STACK) test

run:
	$(STACK) run
