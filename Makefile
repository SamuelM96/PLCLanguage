all: mysplinterpreter

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll
	ocamllex $<

mysplinterpreter: interpreter.ml lexer.ml parser.ml ast.mli
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -o $@ str.cma parser.ml lexer.ml interpreter.ml

clean:
	rm -f ast.cmo
	rm -f ast.cmi
	rm -f lexer.cmo
	rm -f lexer.cmi
	rm -f parser.cmo
	rm -f parser.cmi
	rm -f parser.mli
	rm -f interpreter.cmo
	rm -f interpreter.cmi
	rm -f lexer.ml
	rm -f parser.ml
	rm -f mysplinterpreter