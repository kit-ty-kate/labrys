DOC = doc

OCB = ocamlbuild -use-ocamlfind
TARGET = src/main.native

SUBSTS = \
	 src/config.ml \

TESTS = \
	tests/basic/basic.t \
	tests/old-examples/old-examples.t \
	tests/examples/examples.t \


all: $(SUBSTS)
	$(OCB) $(TARGET)

$(SUBSTS): $(addsuffix .in, $(SUBSTS))
	opam config subst $(SUBSTS)

clean:
	$(OCB) -clean
	@$(RM) $(SUBSTS)

semantics: $(DOC)/semantics.ott
	ott -i $< -o $(<:.ott=.tex) \
	    && rubber --pdf --into $(DOC) $(<:.ott=.tex) \
	    && $(RM) $(<:.ott=.aux) $(<:.ott=.log) $(<:.ott=.tex)

stdlib:
	./main.native build-module --no-prelude --src-dir stdlib Prelude

tests:
	CERVOISE=$(shell pwd) LLVM_VERSION=3.8 cram $(TESTS)

check:
	dead_code_analyzer.opt --all -S -bind-seq --exclude _build/src/parsing/parser.ml _build/src

.PHONY: all clean semantics stdlib tests check
