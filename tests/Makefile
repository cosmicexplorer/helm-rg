EMACS=emacs

.PHONY: test

test: compile-all checkdoc
	$(EMACS) -Q --batch $(CL) \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-l ert -l ../helm-rg.elc -l helm-rg-test.elc \
		--eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

install-packages:
	$(EMACS) -Q --batch -l install-packages.el

checkdoc:
	$(EMACS) -Q --batch $(CL) -l checkdoc-batch.el 2>&1 \
	| grep -vF 'Some lines are over 80 columns wide' \
	| grep -E "helm-rg.el:[1-9]+" && exit 1 || exit 0

compile-all:
	$(EMACS) -Q --batch \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-l ../helm-rg.el \
		-f batch-byte-compile ../helm-rg.el helm-rg-test.el

clean:
	rm -f *.elc ../*.elc
