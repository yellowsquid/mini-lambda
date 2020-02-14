.DEFAULT_GOAL := release

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
CC := dune
OUT := lambda

# Dependencies used for developing and testing dune
DEV_DEPS := \
menhir \
merlin \
utop

-include Makefile.dev

release: $(OUT)
	$(CC) build

lambda:
	$(MAKE) -C src lambda

dev: $(OUT)
	$(CC) build @install

all: $(OUT)
	$(CC) build

install:
	$(BIN) install $(INSTALL_ARGS) dune

uninstall:
	$(CC) uninstall $(INSTALL_ARGS) dune

reinstall: uninstall install

dev-switch:
	opam switch create -y . --deps-only --with-test
	opam install -y $(DEV_DEPS)

test: $(OUT)
	$(CC) runtest

check: $(OUT)
	$(CC) build @check

fmt: $(OUT)
	$(CC) build @fmt --auto-promote

promote: $(OUT)
	$(CC) promote

accept-corrections: promote

clean: $(OUT)
	$(CC) clean || true
	rm -rf _build

distclean: clean
	rm -f src/dune/setup.ml

doc:
	cd doc && sphinx-build . _build

livedoc:
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
	$(CC) $(RUN_ARGS)

.PHONY: default install uninstall reinstall clean test doc dev-switch
.PHONY: promote accept-corrections opam-release dune check fmt
