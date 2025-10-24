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

.PHONY: test promote test-promote test-cc clean release-macos release-linux

default:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe dpt
	dune build src/bin/compiler.exe
	cp -f _build/default/src/bin/compiler.exe dptc
	dune build src/bin/lucidcc.exe
	cp -f _build/default/src/bin/lucidcc.exe lucidcc

mac-release:
	./scripts/builds/build_macos_release.sh

linux-release:
	docker run --rm -v "$(CURDIR)":/build lucid-builder bash -c "eval \$(opam env) && ./scripts/builds/build_linux_release.sh"

all:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe dpt
	dune build src/bin/compiler.exe
	cp -f _build/default/src/bin/compiler.exe dptc
	dune build src/bin/lucidcc.exe
	cp -f _build/default/src/bin/lucidcc.exe lucidcc
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

promote:
	cp test/output/* test/expected/

test-promote: default
	python3 ./test/runtests.py
	cp test/output/* test/expected/

test-cc: default
	python3 ./test/runtests.py --lucidcc

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
