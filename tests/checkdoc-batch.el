(require 'checkdoc)
(require 'cl-lib)

(defun helm-rg-test--checkdoc-file (file)
  "Check FILE for document, comment, error style, and rogue spaces.
Taken from Emacs 25 source."
  (with-current-buffer (find-file-noselect file)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t))))

(setq checkdoc-force-docstrings-flag nil)
(setq sentence-end-double-space nil)

(let ((argv command-line-args))
  (pop argv)                            ; This is the emacs executable.
  (cl-assert (string-equal "-l" (pop argv)))
  (cl-assert (string-equal "tests/checkdoc-batch.el" (pop argv)))
  (cl-loop for file in argv
           do (helm-rg-test--checkdoc-file file)))
