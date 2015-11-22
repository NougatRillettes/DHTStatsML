all:
	ocamlbuild -lib unix tmp.native

profile:
	ocamlbuild  -tag debug -tag profile -lib unix tmp.p.native
