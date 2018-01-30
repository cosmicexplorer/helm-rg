#!/bin/sh
:;exec emacs -batch -l "$0" -- "$@"

(defconst search-regexp
  (format "\\(%s\\)\\(?:%s\\)\\(%s\\)"
          ";;; Commentary:"
          "[[:ascii:]]*?"
          ";; End Commentary"))
(defconst target-file "rg3.el")

(with-current-buffer (find-file target-file)
  (re-search-forward search-regexp)
  (replace-match "\\1\n\\2")
  (save-buffer)
  (kill-buffer))

(let ((res (shell-command-to-string "./create-markdown.coffee")))
  (unless (string= res "")
    (message "%s" res)))

;; Local Variables:
;; mode: emacs-lisp
;; End:
