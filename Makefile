all: mysplinterpreter

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll
	ocamllex $<

mysplinterpreter: compiler.ml lexer.ml parser.ml ast.mli
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -o $@ str.cma parser.ml lexer.ml compiler.ml

clean:
	rm -f ast.cmo
	rm -f ast.cmi
	rm -f lexer.cmo
	rm -f lexer.cmi
	rm -f parser.cmo
	rm -f parser.cmi
	rm -f compiler.exe.cmo
	rm -f compiler.exe.cmi
	rm -f lexer.ml
	rm -f parser.ml
	# rm -f mysplinterpreter
