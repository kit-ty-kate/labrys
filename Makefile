DOC = doc

OCB = ocamlbuild -use-ocamlfind
TARGET = src/main.native
SUBSTS = \
	 src/config.ml

all: $(SUBSTS)
	$(OCB) $(TARGET)

$(SUBSTS):
	opam config subst $(SUBSTS)

clean:
	$(OCB) -clean
	@$(RM) $(SUBSTS)

semantics: $(DOC)/semantics.ott
	ott -i $< -o $(<:.ott=.tex) \
	    && rubber --pdf --into $(DOC) $(<:.ott=.tex) \
	    && $(RM) $(<:.ott=.aux) $(<:.ott=.log) $(<:.ott=.tex)

stdlib:
	./main.native build-module --no-prelude --src-dir stdlib --build-dir . Prelude
	./main.native build-module --src-dir stdlib --build-dir . Nat

.PHONY: all clean semantics stdlib
