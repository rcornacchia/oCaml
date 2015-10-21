TARFILES = Makefile scanner.mll parser.mly ast.mli calc.ml

OBJS = parser.cmo scanner.cmo calc.cmo

calc : $(OBJS)
	ocamlc -o calc $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

calculator.tar.gz : $(TARFILES)
	cd .. && tar zcf calculator/calculator.tar.gz $(TARFILES:%=calculator/%)

.PHONY : clean
clean :
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
calc.cmo: scanner.cmo parser.cmi ast.cmi
calc.cmx: scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
