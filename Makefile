play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

compile:
	ocamlbuild -use-ocamlfind data.cmo main.cmo

zip:
	zip final.zip *.mli *.ml _tags .merlin *.md Makefile

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

clean:
	ocamlbuild -clean
	rm final.zip
