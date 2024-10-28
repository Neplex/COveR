# SHELL
SHELL = /bin/sh
R = Rscript

# Directory
SRC_DIR = src/
DOC_DIR = man/
TESTS_DIR = tests/

# File to clean
TRASH = $(SRC_DIR)*.so $(SRC_DIR)*.o $(SRC_DIR)*/*.o $(SRC_DIR)*/*/*.o $(SRC_DIR)*/*/*/*.o *.tar.gz

# == MAKE COMMAND ==============================================================

.PHONY: all help config doc check checkmem build install test lint clean

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
	@echo "Configuring..."
	@autoconf
	@./configure

doc: config
	$(R) -e devtools::document\(\)

check: doc
	$(R) -e devtools::check\(\)

checkmem: doc
	echo "devtools::check()" | R --vanilla --slave --debugger=valgrind --debugger-args="--leak-check=full --track-origins=yes --log-file=valgrind.log"

build: doc
	$(R) -e devtools::build\(\)

install: doc
	$(R) -e devtools::install\(\)

test: doc
	$(R) -e devtools::test\(\)

lint: build
	$(R) -e lintr::lint_package\(\)

clean:
	@rm $(TRASH) 2>/dev/null || true  # Suppress errors if no files are found
	@./cleanup
