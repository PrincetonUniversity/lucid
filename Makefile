include $(shell ocamlfind query visitors)/Makefile.preprocess

.PHONY: test clean

default:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe lucid_interp
	dune build src/bin/compiler.exe
	cp -f _build/default/src/bin/compiler.exe lucid


generatedVisitors: src/lib/frontend/Syntax.processed.ml

test: default
	python3 ./test/runtests.py

doc:
	dune build @doc

format:
	find src -type f -regex ".*\.mli*" -exec ocamlformat --inplace {} \;
	find test -type f -regex ".*\.mli*" -exec ocamlformat --inplace {} \;

install-deps:
	opam install -y \
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
	z3

clean:
	dune clean
	rm -f lucid_interp
	rm -f lucid
