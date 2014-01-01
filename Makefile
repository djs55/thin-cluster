.PHONY: all clean install build reinstall uninstall distclean
all: build

NAME=dmthin
J=4

clean:
	@rm -f setup.data setup.log setup.bin config.mk
	@rm -rf _build

configure: configure.ml
	ocamlfind ocamlc -linkpkg -package findlib,cmdliner -o configure configure.ml
	@rm -f configure.cm*

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

install:
	@./setup.bin -install

reinstall:
	@ocamlfind remove $(NAME) || true
	@./setup.bin -install

uninstall:
	@ocamlfind remove $(NAME) || true

