(require 'checkdoc)

(defun helm-rg-test--checkdoc-file (file)
  "Check FILE for document, comment, error style, and rogue spaces.
Taken from Emacs 25 source."
  (with-current-buffer (find-file-noselect file)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t))))

(setq checkdoc-force-docstrings-flag nil)
(setq sentence-end-double-space nil)

(helm-rg-test--checkdoc-file "../helm-rg.el")
