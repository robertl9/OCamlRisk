command:
	ocamlbuild -use-ocamlfind command.byte

state:
	ocamlbuild -use-ocamlfind state.byte

clean:
	ocamlbuild -clean

play: command state
	rm -f gui && rm -f gui.cmo && rm -f gui.cmi && ocamlbuild -use-ocamlfind main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind tests.byte && ./tests.byte -runner sequential

check:
	bash checkenv.sh && bash checktypes.sh
