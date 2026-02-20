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
	dune build src/bin/interpMain.exe
	cp -f _build/default/src/bin/interpMain.exe dpt
	dune build src/bin/lucidp4t.exe
	cp -f _build/default/src/bin/lucidp4t.exe dptc
	dune build src/bin/lucidcc.exe
	cp -f _build/default/src/bin/lucidcc.exe lucidcc
	dune build src/bin/lucidSoftSwitch.exe
	cp -f _build/default/src/bin/lucidSoftSwitch.exe lucidSoftSwitch

mac-release:
	./scripts/builds/build_macos_release.sh

linux-release:
	docker run --rm -v "$(CURDIR)":/build lucid-builder bash -c "eval \$(opam env) && ./scripts/builds/build_linux_release.sh"

all:
	dune build src/bin/interpMain.exe
	cp -f _build/default/src/bin/interpMain.exe dpt
	dune build src/bin/lucidp4t.exe
	cp -f _build/default/src/bin/lucidp4t.exe dptc
	dune build src/bin/lucidcc.exe
	cp -f _build/default/src/bin/lucidcc.exe lucidcc
	mkdir -p bin
	dune build src/bin/dockerUtils.exe
	cp -f _build/default/src/bin/dockerUtils.exe bin/dockerUtils
	dune build src/bin/dfgCompiler.exe
	cp -f _build/default/src/bin/dfgCompiler.exe bin/dfgCompiler
	dune build src/bin/eventParsers.exe
	cp -f _build/default/src/bin/eventParsers.exe bin/eventParsers

switch: 
	dune build src/bin/lucidSoftSwitch.exe
	cp -f _build/default/src/bin/lucidSoftSwitch.exe lucidSwitch

generatedVisitors: src/lib/frontend/Syntax.processed.ml

# interpreter tests
test: default
	python3 ./test/runtests.py

promote:
	cp test/output/* test/expected/

test-promote: default
	python3 ./test/runtests.py
	cp test/output/* test/expected/

# c compiler tests
test-cc: default
	python3 ./test/runtests.py --lucidcc

# tofino compiler tests
# test compilation to P4 only
test-tofino-compile: default 
	cd test/backend && ./compilertests.py testspecs/compile_only.json

# test compilation to P4, P4 assembly, and program-specific test cases
EXPECTED_SDE_VER := bf-sde-9.13.0
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
	rm -f lucidcc
