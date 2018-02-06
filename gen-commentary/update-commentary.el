#!/bin/sh
:;exec emacs -batch -l "$0" -- "$@"

;;; execute this file from within its containing directory to sync helm-rg.el
;;; with the project readme

(defconst search-regexp
  (format "\\(%s\\)\\(?:%s\\)\\(%s\\)"
          ";;; Commentary:"
          "[[:ascii:]]*?"
          ";; End Commentary"))
(defconst target-file "../helm-rg.el")

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
