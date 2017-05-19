DOC = doc

LLVM_VERSION ?= 3.8

TESTS = \
    tests/basic/basic.t \
    tests/old-examples/old-examples.t \
    tests/examples/examples.t \
    tests/errors/errors.t \

DOCS = \
    $(DOC)/semantics.pdf \
    $(DOC)/system-f-omega.pdf \

src/%:
	ocamlbuild -use-ocamlfind -no-plugin "$@"

# NOTE: SOURCE_DATE_EPOCH is set to a custom value for making reproducible builds (see pdflatex(1))

%.pdf: %.ott
	ott -i $< -o $(<:.ott=.tex) -tex_show_meta false
	SOURCE_DATE_EPOCH=0 rubber --pdf --into $(DOC) $(<:.ott=.tex)
	rubber --clean --into $(DOC) $(<:.ott=.tex)
	$(RM) $(<:.ott=.tex)

all:
	ocamlbuild -use-ocamlfind -plugin-tag "package(ocamlbuild-pkg)" cervoise

clean:
	ocamlbuild -clean

docs: $(DOCS)

clean-docs:
	$(RM) $(DOCS)

stdlib:
	./main.native build-module --no-prelude --src-dir stdlib Prelude

tests:
	CERVOISE="$(shell pwd)" LLVM_VERSION="$(LLVM_VERSION)" cram $(TESTS)

check:
	dead_code_analyzer.opt --all -S -bind-seq --exclude _build/src/parsing/parser.ml _build/src

.PHONY: all clean docs stdlib tests check
