.DEFAULT_GOAL := release

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)

SRC_DEPS:=$(wildcard src/*.ml*)
OUT := _build/install/default/bin/lambda

LAMBDA=$(abspath $(OUT))
RUNTIME=$(abspath runtime/runtime)
export LAMBDA
export RUNTIME

# Dependencies used for developing and testing dune
DEV_DEPS := \
menhir \
merlin \
utop

-include Makefile.dev

release: $(OUT)
	dune build

$(OUT): $(SRC_DEPS)
	(cd src && dune build)

dev: $(OUT)
	dune build @install

all: $(OUT)
	dune build

install:
	$(BIN) install $(INSTALL_ARGS) dune

uninstall:
	dune uninstall $(INSTALL_ARGS) dune

reinstall: uninstall install

dev-switch:
	opam switch create -y . --deps-only --with-test
	opam install -y $(DEV_DEPS)

test: $(OUT)
	dune runtest

fulltest: $(OUT)
	$(MAKE) -C full-test

check: $(OUT)
	dune build @check

fmt: $(OUT)
	dune build @fmt --auto-promote

promote: $(OUT)
	dune promote

accept-corrections: promote

clean: $(OUT)
	-dune clean
	-rm -rf _build
	$(MAKE) -C full-test clean

distclean: clean
	-rm -f src/dune/setup.ml

doc:
	cd doc && sphinx-build . _build

livedoc:h
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

# If the first argument is "run"...
ifeq (dune,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

dune: $(OUT)
	dune $(RUN_ARGS)

.PHONY: default install uninstall reinstall clean test doc dev-switch
.PHONY: promote accept-corrections opam-release dune check fmt
.PHONY: fulltest
