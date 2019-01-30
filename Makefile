EMACS:=emacs

# TODO: do interactive testing here too (start up an interactive emacs)!
# TODO: once that is done, invoke EVM here to test against multiple emacs versions!

# TODO: maybe add noninteractive testing in a --batch command if this becomes too slow.
test: compile-all checkdoc
	$(EMACS) -Q \
		--eval '(package-initialize)' \
		-l helm-rg.elc -l tests/helm-rg-test.elc \
                -l ert -l rx \
		--eval "(ert (rx bos \"test-helm-rg\"))" \
		--eval '(kill-emacs 0)'
	@echo "All tests passed!"

# TODO: what is this needed for?
install-packages:
	$(EMACS) -Q --batch -l tests/install-packages.el

error_output:=error-output.log

# Output to a file, and if any errors we care about are detected, print the whole output as well.
checkdoc:
	find . -maxdepth 1 -type f -name 'helm-rg-*.el' \
        | xargs $(EMACS) -Q --batch -l tests/checkdoc-batch.el 2>&1 \
        | tee $(error_output) \
	| grep -vF 'Some lines are over 80 columns wide' \
	| grep -vF 'Arguments occur in the doc string out of order' \
	| grep -E '^.*\.el:[1-9]+|exited with status 255' \
        && (cat $(error_output) >&2 ; exit 1) \
        || exit 0

compile-all:
	$(EMACS) -Q --batch \
		--eval '(package-initialize)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-l helm-rg.el \
		-f batch-byte-compile helm-rg.el tests/helm-rg-test.el

clean:
	find . -type f -name '*.elc' -exec rm '{}' '+'
        rm -f $(error_output)
