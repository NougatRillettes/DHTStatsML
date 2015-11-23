all:
	ocamlbuild -lib unix Main.native

profile:
	ocamlbuild  -tag debug -tag profile -lib unix Main.p.native
