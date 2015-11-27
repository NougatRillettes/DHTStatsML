all:
	ocamlbuild -tag thread -lib unix Main.native

profile:
	ocamlbuild  -tag thread -tag debug -tag profile -lib unix Main.p.native
