DOC = doc

LLVM_VERSION ?= 3.8

DOCS = \
    $(DOC)/semantics.pdf \
    $(DOC)/system-f.pdf \
    $(DOC)/system-fw.pdf \
    $(DOC)/system-fw-with-effects.pdf \
    $(DOC)/system-fc.pdf \
    $(DOC)/system-fcw.pdf \

%.tex: %.ott
	ott -i $< -o $@ -tex_show_meta false

# NOTE: SOURCE_DATE_EPOCH is set to a custom value for making reproducible builds (see pdflatex(1))
%.pdf: %.tex
	SOURCE_DATE_EPOCH=0 rubber --pdf --into $(DOC) $<
	@rubber --clean --into $(DOC) $<
	@$(RM) $<

all:
	dune build

clean:
	dune clean

docs: $(DOCS)

clean-docs:
	@$(RM) $(DOCS)

stdlib:
	dune exec -- cervoise build-module --no-prelude --build-dir . --src-dir stdlib Prelude

tests:
	LLVM_VERSION="$(LLVM_VERSION)" dune runtest $(TESTS)

check:
	dead_code_analyzer.opt --all -S -bind-seq --exclude _build/src/parsing/parser.ml _build/src

.PHONY: all clean docs clean-docs stdlib tests check
