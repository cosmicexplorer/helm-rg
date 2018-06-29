EMACS=emacs

.PHONY: test

test: compile-all # checkdoc
	$(EMACS) -Q --batch $(CL) \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-l ert -l ../helm-rg.elc -l helm-rg-test.elc \
		--eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

checkdoc:
	$(EMACS) -Q --batch $(CL) -l checkdoc-batch.el 2>&1 \
	| grep -E "helm-rg.el:[1-9]+" && exit 1 || exit 0

compile-all:
	$(EMACS) -Q --batch \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-l ../helm-rg.el \
		-f batch-byte-compile ../helm-rg.el helm-rg-test.el

clean:
	rm -f *.elc ../*.elc

evm:
	$(MAKE) test EMACS="$(shell evm bin emacs-24.1)" CL="-l cl-lib-0.5.el"
	$(MAKE) test EMACS="$(shell evm bin emacs-24.2)" CL="-l cl-lib-0.5.el"
	$(MAKE) test EMACS="$(shell evm bin emacs-24.3)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-24.4)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-24.5)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-25.1)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-25.2)" CL=""
