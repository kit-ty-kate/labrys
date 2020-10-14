DOC = doc

LLVM_VERSION ?= $(shell opam var llvm:version | sed -E 's/(.\..)\..$$/\1/')

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

tests:
	LLVM_VERSION=$(LLVM_VERSION) dune runtest -f

check:
	dune build @check @all
	reanalyze.exe -dce-cmt _build/default/src/.main.eobjs/byte
	reanalyze.exe -exception-cmt _build/default/src/.main.eobjs/byte

.PHONY: all clean docs clean-docs tests check
