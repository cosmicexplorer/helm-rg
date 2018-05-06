;;; helm-rg.el --- a helm interface to ripgrep -*- lexical-binding: t -*-
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
;; URL: https://github.com/cosmicexplorer/helm-rg
;; Package-Requires: ((emacs "25") (helm "2.8.8") (cl-lib "0.5") (dash "2.13.0") (pcre2el "1.0"))
;; Keywords: find, file, files, helm, fast, rg, ripgrep, grep, search


;;; Commentary:

;; The below is generated from a README at
;; https://github.com/cosmicexplorer/helm-rg.

;; MELPA: https://melpa.org/#/helm-rg

;; Search massive codebases extremely fast, using `ripgrep'
;; (https://github.com/BurntSushi/ripgrep) and `helm'
;; (https://github.com/emacs-helm/helm). Inspired by `helm-ag'
;; (https://github.com/syohex/emacs-helm-ag) and `f3'
;; (https://github.com/cosmicexplorer/f3).

;; Also check out rg.el (https://github.com/dajva/rg.el), which I haven't used
;; much but seems pretty cool.


;; Usage:

;; *See the `ripgrep' whirlwind tour
;; (https://github.com/BurntSushi/ripgrep#whirlwind-tour) for further
;; information on invoking `ripgrep'.*

;; - Invoke the interactive function `helm-rg' to start a search with `ripgrep'
;; in the current directory.
;;     - `helm' is used to browse the results and update the output as you
;; type.
;;     - Each line has the file path, the line number, and the column number of
;; the start of the match, and each part is highlighted differently.
;;     - Use `TAB' to invoke the helm persistent action, which previews the
;; result and highlights the matched text in the preview.
;;     - Use `RET' to visit the file containing the result, move point to the
;; start of the match, and recenter.
;;         - The result's buffer is displayed with
;; `helm-rg-display-buffer-normal-method' (which defaults to
;; `switch-to-buffer').
;;         - Use a prefix argument (`C-u RET') to open the buffer with
;; `helm-rg-display-buffer-alternate-method' (which defaults to
;; `pop-to-buffer').
;; - The text entered into the minibuffer is interpreted into a PCRE
;; (https://pcre.org) regexp to pass to `ripgrep'.
;;     - `helm-rg''s pattern syntax is basically PCRE, but single spaces
;; basically act as a more powerful conjunction operator.
;;         - For example, the pattern `a b' in the minibuffer is transformed
;; into `a.*b|b.*a'.
;;             - The single space can be used to find lines with any
;; permutation of the regexps on either side of the space.
;;             - Two spaces in a row will search for a literal single space.
;;         - `ripgrep''s `--smart-case' option is used so that case-sensitive
;; search is only on if any of the characters in the pattern are capitalized.
;;             - For example, `ab' (conceptually) searches `[Aa][bB]', but `Ab'
;; in the minibuffer will only search for the pattern `Ab' with `ripgrep',
;; because it has at least one uppercase letter.
;; - Use `M-d' to select a new directory to search from.
;; - Use `M-g' to input a glob pattern to filter files by, e.g. `*.py'.
;;     - The glob pattern defaults to the value of
;; `helm-rg-default-glob-string', which is an empty string (matches every file)
;; unless you customize it.
;;     - Pressing `M-g' again shows the same minibuffer prompt for the glob
;; pattern, with the string that was previously input.
;; - Use `<left>' and `<right>' to go up and down by files in the results.
;;     - `<up>' and `<down>' simply go up and down by match result, and there
;; may be many matches for your pattern in a single file, even multiple on a
;; single line (which `ripgrep' reports as multiple separate results).
;;     - The `<left>' and `<right>' keys will move up or down until it lands on
;; a result from a different file than it started on.
;;         - When moving by file, `helm-rg' will cycle around the results list,
;; but it will print a harmless error message instead of looping infinitely if
;; all results are from the same file.
;; - Use the interactive autoloaded function `helm-rg-display-help' to see the
;; ripgrep command's usage info.


;; TODO:

;; - make a keybinding to drop into an edit mode and edit file content inline
;; in results like `helm-ag' (https://github.com/syohex/emacs-helm-ag)
;; - allow (elisp)? regex searching of search results, including file names
;;     - use `helm-swoop' (https://github.com/ShingoFukuyama/helm-swoop)?
;; - publish `update-commentary.el' and the associated machinery as an npm
;; package


;; License:

;; GPL 3.0+ (./LICENSE)

;; End Commentary


;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dash)
(require 'font-lock)
(require 'helm)
(require 'helm-grep)
(require 'helm-lib)
(require 'pcase)
(require 'pcre2el)
(require 'rx)
(require 'rxt)


;; Helpers
(defun helm-rg--always-safe-local (_)
  "Use as a :safe predicate in a `defcustom' form to accept any local override."
  t)


;; Customization
(defgroup helm-rg nil
  "Group for `helm-rg' customizations."
  :group 'helm-grep)

(defcustom helm-rg-ripgrep-executable (executable-find "rg")
  "The location of the ripgrep binary executable."
  :type 'string
  :group 'helm-rg)

(defcustom helm-rg-default-glob-string ""
  "The glob pattern used for the '-g' argument to ripgrep.
Set to the empty string to match every file."
  :type 'string
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-default-directory 'default
  "Specification for starting directory to invoke ripgrep in.
Used in `helm-rg--interpret-starting-dir'. Possible values:

'default => Use `default-directory'.
'git-root => Use \"git rev-parse --show-toplevel\" (see
             `helm-rg-git-executable').
<string> => Use the directory at path <string>."
  :type '(choice symbol string)
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-git-executable (executable-find "git")
  "Location of git executable."
  :type 'string
  :group 'helm-rg)

(defcustom helm-rg-thing-at-point 'symbol
  "Type of object at point to initialize the `helm-rg' minibuffer input with."
  :type 'symbol
  :group 'helm-rg)

(defcustom helm-rg-input-min-search-chars 3
  "Ripgrep will not be invoked unless the input is at least this many chars.

See `helm-rg--make-process' and `helm-rg--make-dummy-process' if interested."
  :type 'integer
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-display-buffer-normal-method #'switch-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

The default function to invoke to display a visited buffer in some window in
`helm-rg'."
  :type 'function
  :group 'helm-rg)

(defcustom helm-rg-display-buffer-alternate-method #'pop-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

The function will be invoked if a prefix argument is used when visiting a result
in `helm-rg'."
  :type 'function
  :group 'helm-rg)



;; Faces
(defface helm-rg-preview-line-highlight
  '((t (:background "green" :foreground "black")))
  "Face for the line of text matched by the ripgrep process."
  :group 'helm-rg)

(defface helm-rg-preview-match-highlight
  '((t (:background "purple" :foreground "white")))
  "Face of the text matched by the pattern given to the ripgrep process."
  :group 'helm-rg)

(defface helm-rg-cmd-arg-face
  '((t (:foreground "green")))
  "Face of arguments in the ripgrep invocation."
  :group 'helm-rg)


;; Constants
(defconst helm-rg--base-command-args
  '("--vimgrep" "--color=always" "--smart-case")
  "Arguments necessary for functionality on the ripgrep command line.")

(defconst helm-rg--buffer-name "*helm-rg*")
(defconst helm-rg--process-name "*helm-rg--rg*")
(defconst helm-rg--process-buffer-name "*helm-rg--rg-output*")

(defconst helm-rg--error-process-name "*helm-rg--error-process*")
(defconst helm-rg--error-buffer-name "*helm-rg--errors*")

(defconst helm-rg--ripgrep-help-buffer-name "*helm-rg-usage-help*")

(defconst helm-rg--vimgrep-output-line-regexp
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

(defconst helm-rg--persistent-action-display-buffer-method #'switch-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

Let-bound to `helm-rg--display-buffer-method' in `helm-rg--async-persistent-action'.")

(defconst helm-rg--loop-input-pattern-regexp
  (rx
   (:
    (* (char ? ))
    ;; group 1 = single entire element
    (group
     (+
      (|
       (not (in ? ))
       (= 2 ? ))))))
  "Regexp applied iteratively to split the input interpreted by `helm-rg'.")


;; Variables
(defvar helm-rg--append-persistent-buffers nil
  "Whether to record buffers opened during an `helm-rg' session.")

(defvar helm-rg--cur-persistent-bufs nil
  "List of buffers opened temporarily during an `helm-rg' session.")

(defvar helm-rg--current-overlays nil
  "List of overlays used to highlight matches in `helm-rg'.")

(defvar helm-rg--current-dir nil
  "Working directory for the current `helm-rg' session.")

(defvar helm-rg--glob-string nil
  "Glob string used for the current `helm-rg' session.")

(defvar helm-rg--glob-string-history nil
  "History variable for the selection of `helm-rg--glob-string'.")

(defvar helm-rg--input-history nil
  "History variable for the pattern input to the ripgrep process.")

(defvar helm-rg--display-buffer-method nil
  "The method to use to display a buffer visiting a result.
Should accept one argument BUF, the buffer to display.")

(defvar helm-rg--paths-to-search nil
  "List of paths to use in the ripgrep command.
All paths are interpreted relative to the directory ripgrep is invoked from.
When nil, searches from the directory ripgrep is invoked from.
See the documentation for `helm-rg-default-directory'.")


;; Logic
(defun helm-rg--make-dummy-process (input)
  "Make a process that immediately exits to display just a title."
  (let* ((dummy-proc (make-process
                      :name helm-rg--process-name
                      :buffer helm-rg--process-buffer-name
                      :command '("echo")
                      :noquery t))
         (helm-src-name
          (format "no results (input '%s' must be at least %d characters)"
                  input
                  helm-rg-input-min-search-chars)))
    (helm-attrset 'name helm-src-name)
    dummy-proc))

(defun helm-rg--join (sep seq)
  (mapconcat #'identity seq sep))

(defun helm-rg--props (props str)
  (apply #'propertize (append (list str) props)))

(defun helm-rg--make-face (face str)
  (helm-rg--props `(face ,face) str))

(defun helm-rg--propertize-cmd-arg (arg)
  (if (string-match-p (rx space) arg)
      (helm-rg--make-face 'font-lock-string-face (format "'%s'" arg))
    (helm-rg--make-face 'helm-rg-cmd-arg-face arg)))

(defun helm-rg--format-argv-string (argv)
  (->>
   argv
   (-map #'helm-rg--propertize-cmd-arg)
   (helm-rg--join " ")
   (format
    (concat
     (propertize "(" 'face 'highlight)
     "%s"
     (propertize ")" 'face 'highlight)))))

(defun helm-rg--process-paths-to-search (paths)
  (--map (expand-file-name it helm-rg--current-dir) paths))

(defun helm-rg--empty-glob-p (glob-str)
  (or (null glob-str)
      (string-blank-p glob-str)))

(defun helm-rg--construct-argv (pattern)
  "Create an argument list for the ripgrep command.
Uses `defcustom' values, and `defvar' values bound in other functions."
  (cons
   helm-rg-ripgrep-executable
   (append
    helm-rg--base-command-args
    (unless (helm-rg--empty-glob-p helm-rg--glob-string)
      (list "-g" helm-rg--glob-string))
    (list pattern)
    (helm-rg--process-paths-to-search helm-rg--paths-to-search))))

(defun helm-rg--make-process-from-argv (argv)
  (let* ((real-proc (make-process
                     :name helm-rg--process-name
                     :buffer helm-rg--process-buffer-name
                     :command argv
                     :noquery t))
         (helm-src-name
          (format "argv: %s" (helm-rg--format-argv-string argv))))
    (helm-attrset 'name helm-src-name)
    (set-process-query-on-exit-flag real-proc nil)
    real-proc))

(defun helm-rg--make-process ()
  "Invoke ripgrep in `helm-rg--current-dir' with `helm-pattern'.
Make a dummy process if the input is empty with a clear message to the user."
  (let* ((default-directory helm-rg--current-dir)
         (input helm-pattern))
    (if (< (length input) helm-rg-input-min-search-chars)
        (helm-rg--make-dummy-process input)
      (helm-rg--make-process-from-argv
       (helm-rg--construct-argv input)))))

(defun helm-rg--decompose-vimgrep-output-line (line)
  "Parse LINE into its constituent elements, returning a plist."
  (when (string-match helm-rg--vimgrep-output-line-regexp line)
    (let ((matches (cl-mapcar (lambda (ind) (match-string ind line))
                              (number-sequence 1 4))))
      (cl-destructuring-bind (file-path line-no col-no content) matches
        (list
         :file-path file-path
         :line-no (1- (string-to-number line-no))
         :col-no (1- (string-to-number col-no))
         :content content)))))

(defun helm-rg--make-overlay-with-face (beg end face)
  "Generate an overlay in region BEG to END with face FACE."
  (let ((olay (make-overlay beg end)))
    (overlay-put olay 'face face)
    olay))

(defun helm-rg--delete-overlays ()
  "Delete all cached overlays in `helm-rg--current-overlays', and clear it."
  (mapc #'delete-overlay helm-rg--current-overlays)
  (setq helm-rg--current-overlays nil))

(defun helm-rg--get-overlay-columns (elisp-regexp content)
  "Find regions matching ELISP-REGEXP in the string CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward elisp-regexp nil t)
     for (beg end) = (match-data t)
     collect (list :beg (1- beg) :end (1- end)))))

(defun helm-rg--async-action (parsed-output)
  "Visit the file at the line and column specified by CAND.
The match is highlighted in its buffer."
  (let ((default-directory helm-rg--current-dir)
        (helm-rg--display-buffer-method
         (or helm-rg--display-buffer-method
             (if helm-current-prefix-arg helm-rg-display-buffer-alternate-method
               helm-rg-display-buffer-normal-method))))
    (helm-rg--delete-overlays)
    (cl-destructuring-bind (&key file-path line-no col-no content) parsed-output
      (let* ((file-abs-path
              (expand-file-name file-path))
             (buffer-to-display
              (or (find-buffer-visiting file-abs-path)
                  (let ((new-buf (find-file-noselect file-abs-path)))
                    (when helm-rg--append-persistent-buffers
                      (push new-buf helm-rg--cur-persistent-bufs))
                    new-buf)))
             (olay-cols
              (helm-rg--get-overlay-columns
               (rxt-pcre-to-elisp helm-pattern) content)))
        (funcall helm-rg--display-buffer-method buffer-to-display)
        (goto-char (point-min))
        (forward-line line-no)
        (let* ((line-olay
                (helm-rg--make-overlay-with-face
                 (line-beginning-position) (line-end-position)
                 'helm-rg-preview-line-highlight))
               (match-olays
                (-map (-lambda ((&plist :beg beg :end end))
                        (helm-rg--make-overlay-with-face
                         (+ (point) beg) (+ (point) end)
                         'helm-rg-preview-match-highlight))
                      olay-cols)))
          (setq helm-rg--current-overlays
                (cons line-olay match-olays)))
        (forward-char col-no)
        (recenter)))))

(defun helm-rg--async-persistent-action (parsed-output)
  "Visit the file at the line and column specified by CAND.
Call `helm-rg--async-action', but push the buffer corresponding to CAND to
`helm-rg--current-overlays', if there was no buffer visiting it already."
  (let ((helm-rg--append-persistent-buffers t)
        (helm-rg--display-buffer-method helm-rg--persistent-action-display-buffer-method))
    (helm-rg--async-action parsed-output)))

(defun helm-rg--kill-proc-if-live (proc-name)
  "Delete the process named PROC-NAME, if it is alive."
  (let ((proc (get-process proc-name)))
    (when (process-live-p proc)
      (delete-process proc))))

(defun helm-rg--kill-bufs-if-live (&rest bufs)
  "Kill any live buffers in BUFS."
  (mapc
   (lambda (buf)
     (when (buffer-live-p (get-buffer buf))
       (kill-buffer buf)))
   bufs))

(defun helm-rg--unwind-cleanup ()
  "Reset all the temporary state in `defvar's in this package."
  (helm-rg--delete-overlays)
  (cl-loop
   for opened-buf in helm-rg--cur-persistent-bufs
   unless (eq (current-buffer) opened-buf)
   do (kill-buffer opened-buf)
   finally (setq helm-rg--cur-persistent-bufs nil))
  (helm-rg--kill-proc-if-live helm-rg--process-name)
  (helm-rg--kill-bufs-if-live helm-rg--buffer-name
                              helm-rg--process-buffer-name
                              helm-rg--error-buffer-name))

(defun helm-rg--do-helm-rg (rg-pattern)
  "Invoke ripgrep to search for RG-PATTERN, using `helm'."
  (helm :sources '(helm-rg-process-source)
        :buffer helm-rg--buffer-name
        :input rg-pattern
        :prompt "rg pattern: "))

(defun helm-rg--get-thing-at-pt ()
  "Get the object surrounding point, or the empty string."
  (helm-aif (thing-at-point helm-rg-thing-at-point)
      (substring-no-properties it)
    ""))

(defun helm-rg--header-name (src-name)
  (format "%s %s @ %s"
          (propertize "rg" 'face 'bold-italic)
          src-name
          helm-rg--current-dir))

(defun helm-rg--display-to-real (cand)
  (helm-rg--decompose-vimgrep-output-line cand))

(defmacro helm-rg--into-temp-buffer (to-insert &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,to-insert)
     (goto-char (point-min))
     ,@body))

(defun helm-rg--collect-matches (regexp)
  (cl-loop while (re-search-forward regexp nil t)
           collect (match-string 1)))

(defun helm-rg--pattern-transformer (pattern)
  (->>
   (helm-rg--into-temp-buffer pattern
     (helm-rg--collect-matches helm-rg--loop-input-pattern-regexp))
   (--map (replace-regexp-in-string (rx (= 2 ? )) " " it))
   (-permutations)
   (--map (helm-rg--join ".*" it))
   (helm-rg--join "|")))

(defun helm-rg--advance-forward ()
  (interactive)
  (if (helm-end-of-source-p)
      (helm-beginning-of-buffer)
    (helm-next-line)))

(defun helm-rg--advance-backward ()
  (interactive)
  (if (helm-beginning-of-source-p)
      (helm-end-of-buffer)
    (helm-previous-line)))

(defun helm-rg--iterate-results (direction success-fn failure-fn)
  (with-helm-window
    (let ((helm-move-to-line-cycle-in-source t)
          (move-fn
           (pcase-exhaustive direction
             (`forward #'helm-rg--advance-forward)
             (`backward #'helm-rg--advance-backward))))
      (call-interactively move-fn)
      (cl-loop

       for cur-line-parsed = (helm-rg--decompose-vimgrep-output-line
                               (helm-current-line-contents))
       until (funcall success-fn cur-line-parsed)
       if (funcall failure-fn cur-line-parsed)
       return (message "%s" "failure: could not cycle to next entry")
       else do (call-interactively move-fn)))))

(defun helm-rg--move-file (direction)
  (-let* ((cur-line
           (with-helm-buffer (helm-current-line-contents)))
          (parsed-line
           (helm-rg--decompose-vimgrep-output-line cur-line))
          ((&plist :file-path file-path
                   :line-no line-no
                   :col-no col-no)
           parsed-line))
    (helm-rg--iterate-results
     direction
     (-lambda ((&plist :file-path new-file-path))
       (not (string-equal new-file-path file-path)))
     (-lambda ((&plist :file-path new-file-path
                       :line-no new-line-no
                       :col-no new-col-no))
       (and
        (string-equal new-file-path file-path)
        (= new-line-no line-no)
        (= new-col-no col-no))))))

(defun helm-rg--file-forward ()
  (interactive)
  (helm-rg--move-file 'forward))

(defun helm-rg--file-backward ()
  (interactive)
  (helm-rg--move-file 'backward))


;; Toggles and settings
(defmacro helm-rg--run-after-exit (&rest body)
  "Wrap BODY in `helm-run-after-exit'."
  `(helm-run-after-exit (lambda () ,@body)))

(defun helm-rg--set-glob ()
  "Set the glob string used to invoke ripgrep and search again."
  (interactive)
  (let* ((pat helm-pattern)
         (start-dir helm-rg--current-dir))
    (helm-rg--run-after-exit
     (let ((helm-rg--current-dir start-dir)
           (helm-rg--glob-string
            (read-string
             "rg glob: " helm-rg--glob-string 'helm-rg--glob-string-history)))
       (helm-rg--do-helm-rg pat)))))

(defun helm-rg--set-dir ()
  "Set the directory in which to invoke ripgrep and search again."
  (interactive)
  (let ((pat helm-pattern))
    (helm-rg--run-after-exit
     (let ((helm-rg--current-dir
            (read-directory-name "rg directory: " helm-rg--current-dir nil t)))
       (helm-rg--do-helm-rg pat)))))

(defun helm-rg--is-executable-file (path)
  (and path
       (file-executable-p path)
       (not (file-directory-p path))))

(defun helm-rg--process-output (exe &rest args)
  "Get output from a process specified by string arguments.
Merges stdout and stderr, and trims whitespace from the result."
  (with-temp-buffer
    (let ((proc (make-process
                 :name "temp-proc"
                 :buffer (current-buffer)
                 :command `(,exe ,@args)
                 :sentinel #'ignore)))
      (while (accept-process-output proc nil nil t)))
    (trim-whitespace (buffer-string))))

(defun helm-rg--check-directory-path (path)
  (if (and path (file-directory-p path)) path
    (error "path '%S' was not a directory." path)))

(defun helm-rg--get-git-root ()
  (if (helm-rg--is-executable-file helm-rg-git-executable)
      (helm-rg--process-output helm-rg-git-executable
                               "rev-parse" "--show-toplevel")
    (error "helm-rg-git-executable is not an executable file (was: %S)."
           helm-rg-git-executable)))

(defun helm-rg--interpret-starting-dir (default-directory-spec)
  (pcase-exhaustive default-directory-spec
    ('default default-directory)
    ('git-root (helm-rg--get-git-root))
    ((pred stringp) (helm-rg--check-directory-path))))

(defun helm-rg--make-help-buffer (help-buf-name)
  (with-current-buffer (get-buffer-create help-buf-name)
    (read-only-mode -1)
    (erase-buffer)
    (fundamental-mode)
    (insert (helm-rg--process-output helm-rg-ripgrep-executable "--help"))
    (goto-char (point-min))
    (read-only-mode 1)
    (current-buffer)))


;; Keymap
(defconst helm-rg-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g") #'helm-rg--set-glob)
    (define-key map (kbd "M-d") #'helm-rg--set-dir)
    (define-key map (kbd "<right>") #'helm-rg--file-forward)
    (define-key map (kbd "<left>") #'helm-rg--file-backward)
    map)
  "Keymap for `helm-rg'.")


;; Helm sources
(defconst helm-rg-process-source
  (helm-make-source "ripgrep" 'helm-grep-ag-class
    :header-name #'helm-rg--header-name
    :keymap 'helm-rg-map
    :history 'helm-rg--input-history
    :help-message "FIXME: useful help message!!!"
    :candidates-process #'helm-rg--make-process
    :action (helm-make-actions "Visit" #'helm-rg--async-action)
    :filter-one-by-one #'ansi-color-apply
    :display-to-real #'helm-rg--display-to-real
    :pattern-transformer #'helm-rg--pattern-transformer
    :persistent-action #'helm-rg--async-persistent-action
    :persistent-help "Visit result buffer and highlight matches"
    :requires-pattern nil
    :group 'helm-rg)
  "Helm async source to search files in a directory using ripgrep.")


;; Autoloaded functions
;;;###autoload
(defun helm-rg (rg-pattern &optional pfx paths)
  "Search for the PCRE regexp RG-PATTERN extremely quickly with ripgrep.

When invoked interactively with a prefix argument, or when PFX is non-nil,
set the cwd for the ripgrep process to `default-directory'. Otherwise use the
cwd as described by `helm-rg-default-directory'.

If PATHS is non-nil, ripgrep will search only those paths, relative to the
process's cwd. Otherwise, the process's cwd will be searched.

Note that ripgrep respects glob patterns from .gitignore, .rgignore, and .ignore
files. This composes with the glob defined by `helm-rg-default-glob-string', or
overridden with `helm-rg--set-glob', which is defined in `helm-rg-map':

\\{helm-rg-map}"
  (interactive (list (helm-rg--get-thing-at-pt) current-prefix-arg nil))
  (let* ((helm-rg--current-dir
          (or helm-rg--current-dir
              (and pfx default-directory)
              (helm-rg--interpret-starting-dir helm-rg-default-directory)))
         (helm-rg--glob-string
          (or helm-rg--glob-string
              helm-rg-default-glob-string))
         (helm-rg--paths-to-search
          (or helm-rg--paths-to-search
              paths)))
    (unwind-protect (helm-rg--do-helm-rg rg-pattern)
      (helm-rg--unwind-cleanup))))

;;;###autoload
(defun helm-rg-display-help (&optional pfx)
  "Display a buffer with the ripgrep command's usage help.

The help buffer will be reused if it was already created. A prefix argument when
invoked interactively, or a non-nil value for PFX, will display the help buffer
in the current window. Otherwise, if the help buffer is already being displayed
in some window, select that window, or else display the help buffer with
`pop-to-buffer'."
  (interactive "P")
  (let ((filled-out-help-buf
         (or (get-buffer helm-rg--ripgrep-help-buffer-name)
             (helm-rg--make-help-buffer helm-rg--ripgrep-help-buffer-name))))
    (if pfx (switch-to-buffer filled-out-help-buf)
      (-if-let ((buf-win (get-buffer-window filled-out-help-buf t)))
          (select-window buf-win)
        (pop-to-buffer filled-out-help-buf)))))

(provide 'helm-rg)
;;; helm-rg.el ends here
