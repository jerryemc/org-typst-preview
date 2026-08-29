EMACS ?= emacs

.PHONY: check compile test checkdoc clean

check: compile test checkdoc

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile org-typst-preview.el

test:
	$(EMACS) -Q --batch -L . -L test \
	  -l test/org-typst-preview-test.el \
	  -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch -L . \
	  --eval "(progn (require 'checkdoc) (checkdoc-file \"org-typst-preview.el\"))"

clean:
	rm -f org-typst-preview.elc test/*.elc
