all:
	ocamlbuild -lib unix tmp.native

profile:
	ocamlbuild -lib unix tmp.p.native
