# SHELL
SHELL = /bin/sh
R = Rscript

# Directory
SRC_DIR = src/
DOC_DIR = man/
TESTS_DIR = tests/

# File to clean
TRASH = $(SRC_DIR)*.so $(SRC_DIR)*.o *.tar.gz

# == MAKE COMMAND ==============================================================

all: build

help:
	@echo "Available:"
	@echo "- config"
	@echo "- doc"
	@echo "- check"
	@echo "- checkmem"
	@echo "- build"
	@echo "- install"
	@echo "- test"
	@echo "- lint"
	@echo "- clean"

config: clean
	autoconf
	./configure

doc: config
	$(R) -e devtools::document\(\)

check: doc
	$(R) -e devtools::check\(\)

checkmem: doc
	echo "devtools::check()" | R --debugger=valgrind --debugger-args="--leak-check=full --track-origins=yes" --vanilla --slave

build: doc
	$(R) -e devtools::build\(\)

install: doc
	$(R) -e devtools::install\(\)

test: doc
	$(R) -e devtools::test\(\)

lint: build
	$(R) -e lintr::lint_package\(\)

.PHONY: clean
clean:
	rm -rf $(TRASH)
	./cleanup
