# include $(shell ocamlfind query visitors)/Makefile.preprocess

DEPENDENCIES = \
integers \
batteries \
ounit \
ansiterminal \
menhir \
ppx_deriving \
ppx_string_interpolation \
zarith \
visitors \
fileutils \
ppx_import \
core \
dune \
ocamlgraph \
angstrom \
yojson \
pyml \
pprint \
z3

DEV_DEPENDENCIES = \
merlin \
ocamlformat

.PHONY: test promote test-promote clean

default:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe dpt
	dune build src/bin/compiler.exe
	cp -f _build/default/src/bin/compiler.exe dptc

c:
	dune build src/bin/cCompiler.exe
	cp -f _build/default/src/bin/cCompiler.exe lucidcc

all:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe dpt
	dune build src/bin/compiler.exe
	cp -f _build/default/src/bin/compiler.exe dptc
	mkdir -p bin
	dune build src/bin/dockerUtils.exe
	cp -f _build/default/src/bin/dockerUtils.exe bin/dockerUtils
	dune build src/bin/dfgCompiler.exe
	cp -f _build/default/src/bin/dfgCompiler.exe bin/dfgCompiler
	dune build src/bin/eventParsers.exe
	cp -f _build/default/src/bin/eventParsers.exe bin/eventParsers

generatedVisitors: src/lib/frontend/Syntax.processed.ml

test: default
	python3 ./test/runtests.py

EXPECTED_SDE_VER := bf-sde-9.13.0
# cd into test/backend and then call ./runtests.sh
test_tofino: default
	@if [ -z "$$SDE" ]; then \
		echo "Error: P4studio SDE directory environment variable (\$$SDE) is not set"; \
		exit 1; \
	fi
	@if [ ! -f "$$SDE/$(EXPECTED_SDE_VER).manifest" ]; then \
		echo "Error: The Lucid-Tofino backend is only tested on SDE $(EXPECTED_SDE_VER), and your \$$SDE directory ($$SDE) does not have a manifest file indicating that the correct version is installed."; \
		exit 1; \
	fi
	cd test/backend && ./runtests.sh

promote:
	cp test/output/* test/expected/

test-promote: default
	python3 ./test/runtests.py
	cp test/output/* test/expected/

doc:
	dune build @doc

format:
	find src -type f -regex ".*\.mli*" -exec ocamlformat --inplace {} \;
	find test -type f -regex ".*\.mli*" -exec ocamlformat --inplace {} \;

install-deps:
	opam install -y $(DEPENDENCIES)

install-dev:
	opam install -y $(DEV_DEPENDENCIES) $(DEPENDENCIES)

clean:
	dune clean
	rm -f dpt
	rm -f dptc
	rm -f test/testing.exe
