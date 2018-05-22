DOC = doc

LLVM_VERSION ?= 3.8

TESTS = \
    tests/basic/basic.t \
    tests/old-examples/old-examples.t \
    tests/examples/examples.t \
    tests/errors/errors.t \

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
	jbuilder build

clean:
	jbuilder clean

docs: $(DOCS)

clean-docs:
	@$(RM) $(DOCS)

stdlib:
	jbuilder exec -- cervoise build-module --no-prelude --build-dir . --src-dir stdlib Prelude

tests:
	CERVOISE="$(shell pwd)" LLVM_VERSION="$(LLVM_VERSION)" cram $(TESTS)

check:
	dead_code_analyzer.opt --all -S -bind-seq --exclude _build/src/parsing/parser.ml _build/src

.PHONY: all clean docs clean-docs stdlib tests check
