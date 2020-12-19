all: test
	ocamlrun ./test
test: interprete.cmo tests.cmo
	ocamlc interprete.cmo tests.cmo -o test
tests.cmo: interprete.cmo tests.ml
	ocamlc -c tests.ml
interprete.cmo: interprete.ml
	ocamlc -c interprete.ml
