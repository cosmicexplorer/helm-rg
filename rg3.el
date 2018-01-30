;;; rg3.el --- a helm interface to ripgrep -*- lexical-binding: t -*-
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author: Danny McClanahan
;; Version: 0.1
;; URL: https://github.com/cosmicexplorer/rg3
;; Package-Requires: ((emacs "25") (helm "2.8.8") (cl-lib "0.5"))
;; Keywords: find, file, files, helm, fast, rg, ripgrep, grep, search


;;; Commentary:

;; The below is generated from a README at
;; https://github.com/cosmicexplorer/rg3.

;; RipGrep Goes Great with Emacs. Search directories fast, using `ripgrep' and
;; `helm'. Inspired by `helm-ag' and `f3'.


;; Usage:

;; *See the `ripgrep' whirlwind tour for further information on invoking
;; `ripgrep'.*

;; - Invoke the interactive function `rg3' to start a search with `ripgrep' in
;; the current directory.
;; - `helm' is used to browse the results and update the output as you
;; type.
;; - Each line has the file path, the line number, and the column number of the
;; start of the match, and each part is highlighted differently.
;; - Use 'TAB' to invoke the helm persistent action, which previews the result
;; and highlights the matched text in the preview.
;; - Use 'RET' to visit the file containing the result, move point to the start
;; of the match, and recenter.
;; - The text entered into the minibuffer is interpreted as a PCRE regexp which
;; `ripgrep' uses to search your files.
;; - Use 'M-d' to select a new directory to search from.
;; - Use 'M-g' to input a glob pattern to filter files by, e.g. `*.py'.
;; - The glob pattern defaults to the value of `rg3-default-glob-string', which
;; is an empty string (matches every file) unless you customize it.
;; - Pressing 'M-g' again shows the same minibuffer prompt for the glob
;; pattern, with the string that was previously input.


;; License:

;; GPL 3.0+

;; End Commentary


;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'rx)


;; Helpers
(defun rg3--always-safe-local (_)
  "Use as a :safe predicate in a `defcustom' form to accept any local override."
  t)


;; Customization
(defgroup rg3 nil
  "Group for `rg3' customizations."
  :group 'files)

(defcustom rg3-base-command '("rg" "--vimgrep" "--color=always")
  "The beginning of the command line to invoke ripgrep, as a list."
  :type 'list
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-candidate-limit 2000
  "The number of lines of output to show at once when running `rg3'."
  :type 'integer
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-default-glob-string ""
  "The glob pattern used for the '-g' argument to ripgrep.
Set to the empty string to match every file."
  :type 'string
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-thing-at-point 'symbol
  "Type of object at point to initialize the `rg3' minibuffer input with."
  :type 'symbol
  :group 'rg3)

(defface rg3-preview-line-highlight
  '((t (:background "green" :foreground "black")))
  "Face for the line of text matched by the ripgrep process."
  :group 'rg3)

(defface rg3-preview-match-highlight
  '((t (:background "purple" :foreground "white")))
  "Face of the text matched by the pattern given to the ripgrep process."
  :group 'rg3)


;; Constants
(defconst rg3--helm-buffer-name "*rg3*")
(defconst rg3--process-name "*rg3--rg*")
(defconst rg3--process-buffer-name "*rg3--rg-output*")

(defconst rg3--error-process-name "*rg3--error-process*")
(defconst rg3--error-buffer-name "*rg3--errors*")

(defconst rg3--vimgrep-output-line-regexp
  (rx (: bol
         (group (+ (not (any ?:))))
         ":"
         (group (+ (any digit)))
         ":"
         (group (+ (any digit)))
         ":"
         (group (* nonl))
         eol))
  "Regexp matching the output of invoking ripgrep with the '--vimgrep' option.")

(defconst rg3--case-insensitive-pattern-regexp
  (rx (: bos (* (not upper)) eos))
  "Regexp matching an search which should be interpreted case-insensitively.")


;; Variables
(defvar rg3--append-persistent-buffers nil
  "Whether to record buffers opened during an `rg3' session.")

(defvar rg3--currently-opened-persistent-buffers nil
  "List of buffers opened temporarily during an `rg3' session.")

(defvar rg3--current-overlays nil
  "List of overlays used to highlight matches in `rg3'.")

(defvar rg3--current-dir nil
  "Working directory for the current `rg3' session.")

(defvar rg3--glob-string nil
  "Glob string used for the current `rg3' session.")

(defvar rg3--glob-string-history nil
  "History variable for the selection of `rg3--glob-string'.")


;; Logic
(defun rg3--make-dummy-process ()
  "Make a process that immediately exits to display just a title."
  (let* ((dummy-proc (make-process
                      :name rg3--process-name
                      :buffer rg3--process-buffer-name
                      :command '("echo")
                      :noquery t))
         (helm-src-name
          (format "rg empty dummy process (no output) @ %s"
                  rg3--current-dir)))
    (helm-attrset 'name helm-src-name)
    dummy-proc))

(defun rg3--make-process ()
  "Invoke ripgrep in `rg3--current-dir' with `helm-pattern'."
  (let ((default-directory rg3--current-dir))
    (if (string= "" helm-pattern)
        (rg3--make-dummy-process)
      (let* ((case-insensitive-p
              (let ((case-fold-search nil))
                (string-match-p
                 rg3--case-insensitive-pattern-regexp helm-pattern)))
             (rg-cmd
              (append
               rg3-base-command
               (list "-g" rg3--glob-string)
               (if case-insensitive-p '("-i") ())
               (list helm-pattern)))
             (real-proc (make-process
                         :name rg3--process-name
                         :buffer rg3--process-buffer-name
                         :command rg-cmd
                         :noquery t))
             (helm-src-name
              (format "rg cmd: '%s' @ %s"
                      (mapconcat (lambda (s) (format "'%s'" s))
                                 rg-cmd " ")
                      rg3--current-dir)))
        (helm-attrset 'name helm-src-name)
        (set-process-query-on-exit-flag real-proc nil)
        real-proc))))

(defun rg3--decompose-vimgrep-output-line (line)
  "Parse LINE into its constituent elements, returning a plist."
  (when (string-match rg3--vimgrep-output-line-regexp line)
    (let ((matches (cl-mapcar (lambda (ind) (match-string ind line))
                              (number-sequence 1 4))))
      (cl-destructuring-bind (file-path line-no col-no content) matches
        (list
         :file-path file-path
         :line-no (1- (string-to-number line-no))
         :col-no (1- (string-to-number col-no))
         :content content)))))

(defun rg3--pcre-to-elisp-regexp (pcre)
  "Convert the string PCRE to an Emacs Lisp regexp."
  (with-temp-buffer
    (insert pcre)
    (goto-char (point-min))
    ;; convert (, ), {, }, |
    (while (re-search-forward "[(){}|]" nil t)
      (backward-char 1)
      (cond ((looking-back "\\\\\\\\" nil))
            ((looking-back "\\\\" nil)
             (delete-char -1))
            (t
             (insert "\\")))
      (forward-char 1))
    ;; convert \s and \S -> \s- \S-
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\s\\)" nil t)
      (unless (looking-back "\\\\\\\\s" nil)
        (insert "-")))
    (buffer-string)))

(defun rg3--make-overlay-with-face (beg end face)
  "Generate an overlay in region BEG to END with face FACE."
  (let ((olay (make-overlay beg end)))
    (overlay-put olay 'face face)
    olay))

(defun rg3--delete-overlays ()
  "Delete all cached overlays in `rg3--current-overlays', and clear it."
  (cl-mapc #'delete-overlay rg3--current-overlays)
  (setq rg3--current-overlays nil))

(defun rg3--get-overlay-columns (elisp-regexp content)
  "Find regions matching ELISP-REGEXP in the string CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward elisp-regexp nil t)
     for (beg end) = (match-data t)
     collect (list :beg (1- beg) :end (1- end)))))

(defun rg3--async-action (cand)
  "Visit the file at the line and column specified by CAND.
The match is highlighted in its buffer."
  (let ((default-directory rg3--current-dir))
    (rg3--delete-overlays)
    (cl-destructuring-bind (&key file-path line-no col-no content)
        (rg3--decompose-vimgrep-output-line cand)
      (let* ((file-abs-path
              (expand-file-name file-path))
             (buffer-to-display
              (or (find-buffer-visiting file-abs-path)
                  (let ((new-buf (find-file-noselect file-abs-path)))
                    (when rg3--append-persistent-buffers
                      (push new-buf rg3--currently-opened-persistent-buffers))
                    new-buf)))
             (olay-cols
              (rg3--get-overlay-columns
               (rg3--pcre-to-elisp-regexp helm-pattern)
               content)))
        (pop-to-buffer buffer-to-display)
        (goto-char (point-min))
        (forward-line line-no)
        (let* ((line-olay
                (rg3--make-overlay-with-face
                 (line-beginning-position)
                 (line-end-position)
                 'rg3-preview-line-highlight))
               (match-olays
                (cl-loop
                 for el in olay-cols
                 collect (cl-destructuring-bind (&key beg end) el
                           (let* ((pt (point))
                                  (pt-beg (+ pt beg))
                                  (pt-end (+ pt end)))
                             (rg3--make-overlay-with-face
                              pt-beg pt-end 'rg3-preview-match-highlight))))))
          (setq rg3--current-overlays
                (append (list line-olay) match-olays)))
        (forward-char col-no)
        (recenter)))))

(defun rg3--async-persistent-action (cand)
  "Visit the file at the line and column specified by CAND.
Call `rg3--async-action', but push the buffer corresponding to CAND to
`rg3--current-overlays', if there was no buffer visiting it already."
  (let ((rg3--append-persistent-buffers t))
    (rg3--async-action cand)))

(defun rg3--kill-proc-if-live (proc-name)
  "Delete the process named PROC-NAME, if it is alive."
  (let ((proc (get-process proc-name)))
    (when (process-live-p proc)
      (delete-process proc))))

(defun rg3--kill-bufs-if-live (&rest bufs)
  "Kill any live buffers in BUFS."
  (cl-mapc
   (lambda (buf)
     (when (buffer-live-p (get-buffer buf))
       (kill-buffer buf)))
   bufs))

(defun rg3--unwind-cleanup ()
  "Reset all the temporary state in `defvar's in this package."
  (rg3--delete-overlays)
  (cl-loop
   for opened-buf in rg3--currently-opened-persistent-buffers
   unless (eq (current-buffer) opened-buf)
   do (kill-buffer opened-buf)
   finally (setq rg3--currently-opened-persistent-buffers nil))
  (rg3--kill-proc-if-live rg3--process-name)
  (rg3--kill-bufs-if-live rg3--helm-buffer-name
                          rg3--process-buffer-name
                          rg3--error-buffer-name))

(defun rg3--do-rg3 (rg-pattern)
  "Invoke ripgrep to search for RG-PATTERN, using `helm'."
  (helm :sources '(rg3--process-source)
        :buffer rg3--helm-buffer-name
        :input rg-pattern
        :prompt "rg pattern: "))

(defun rg3--get-thing-at-pt ()
  "Get the object surrounding point, or the empty string."
  (helm-aif (thing-at-point rg3-thing-at-point)
      (substring-no-properties it)
    ""))


;; Toggles and settings
(defmacro rg3--run-after-exit (&rest body)
  "Wrap BODY in `helm-run-after-exit'."
  `(helm-run-after-exit (lambda () ,@body)))

(defun rg3--set-glob ()
  "Set the glob string used to invoke ripgrep and search again."
  (interactive)
  (let* ((pat helm-pattern)
         (start-dir rg3--current-dir))
    (rg3--run-after-exit
     (let ((rg3--current-dir start-dir)
           (rg3--glob-string
            (read-string
             "rg glob: " rg3--glob-string 'rg3--glob-string-history)))
       (rg3--do-rg3 pat)))))

(defun rg3--set-dir ()
  "Set the directory in which to invoke ripgrep and search again."
  (interactive)
  (let ((pat helm-pattern))
    (rg3--run-after-exit
     (let ((rg3--current-dir
            (read-directory-name "rg directory: " rg3--current-dir nil t)))
       (rg3--do-rg3 pat)))))


;; Keymap
(defconst rg3-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g") #'rg3--set-glob)
    (define-key map (kbd "M-d") #'rg3--set-dir)
    map)
  "Keymap for `rg3'.")


;; Helm sources
(defconst rg3--process-source
  (helm-build-async-source "ripgrep"
    :candidates-process #'rg3--make-process
    :candidate-number-limit rg3-candidate-limit
    :action (helm-make-actions "Visit" #'rg3--async-action)
    :filter-one-by-one #'ansi-color-apply
    :persistent-action #'rg3--async-persistent-action
    :keymap 'rg3-map)
  "Helm async source to search files in a directory using ripgrep.")


;; Autoloaded functions
;;;###autoload
(defun rg3 (rg-pattern)
  "Search for the perl regexp RG-PATTERN extremely quickly with ripgrep.

\\{rg3-map}"
  (interactive (list (rg3--get-thing-at-pt)))
  (let* ((rg3--current-dir (or rg3--current-dir default-directory))
         (rg3--glob-string (or rg3--glob-string rg3-default-glob-string)))
    (unwind-protect (rg3--do-rg3 rg-pattern)
      (rg3--unwind-cleanup))))

(provide 'rg3)
;;; rg3.el ends here
