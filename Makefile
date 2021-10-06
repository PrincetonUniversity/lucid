# include $(shell ocamlfind query visitors)/Makefile.preprocess

.PHONY: test clean



default:
	dune build src/bin/main.exe
	cp -f _build/default/src/bin/main.exe dpt

generatedVisitors: src/lib/frontend/Syntax.processed.ml

#install: default
#	cp _build/default/src/bin/main.exe dpt

# test: default
# 	dune runtest -f --no-buffer
# test:
# 	dune build test/testing.exe
# 	cp _build/default/test/testing.exe test
test: default
	python3 ./test/runtests.py

evaluate: default
	python3 ./test/aec_evaluate.py

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
	rm -f dpt
	rm -f dptc
	rm -f test/testing.exe
