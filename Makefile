all: test
	ocamlrun ./test
test: linguaggio.cmo interprete.cmo tests.cmo
	ocamlc -o test linguaggio.cmo interprete.cmo tests.cmo
tests.cmo: linguaggio.cmo interprete.cmo tests.ml
	ocamlc -c linguaggio.cmo interprete.cmo tests.ml
interprete.cmo: linguaggio.cmo interprete.ml
	ocamlc -c linguaggio.cmo interprete.ml
linguaggio.cmo: linguaggio.ml
	ocamlc -c linguaggio.ml
clean:
	rm -f test linguaggio.cmo linguaggio.cmi interprete.cmo interprete.cmi tests.cmo tests.cmi
