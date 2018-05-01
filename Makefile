gui:

	rm -f gui && rm -f gui.cmo && rm -f gui.cmi && ocamlbuild -clean && ocamlc -I +../lablgtk2 -o gui lablgtk.cma gtkInit.cmo gui.ml && ./gui

clean: 
	rm -f gui && rm -f gui.cmo && rm -f gui.cmi && ocamlbuild -clean

play: 
	rm -f gui && rm -f gui.cmo && rm -f gui.cmi && ocamlbuild -use-ocamlfind main.byte && ./main.byte

test:	
	ocamlbuild -use-ocamlfind tests.byte && tests.byte
	


