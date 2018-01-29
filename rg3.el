;;; -*- lexical-binding: t -*-


;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dash)
(require 'helm)
(require 'helm-lib)
(require 'rx)


;; Helpers
(defun rg3--always-safe-local (_) t)


;; Customization
(defgroup rg3 nil
  "???"
  :group 'helm)

(defcustom rg3-base-command '("rg" "--vimgrep" "--color=always")
  "???"
  :type 'list
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-candidate-limit 2000
  "???"
  :type 'integer
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-default-glob-string ""
  "???"
  :type 'string
  :safe #'rg3--always-safe-local
  :group 'rg3)

(defcustom rg3-thing-at-point 'symbol
  "???"
  :type 'symbol
  :group 'rg3)

(defface rg3-preview-line-highlight
  '((t (:background "green" :foreground "black")))
  "???"
  :group 'rg3)

(defface rg3-preview-match-highlight
  '((t (:background "purple" :foreground "white")))
  "???"
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
         eol)))

(defconst rg3--case-insensitive-pattern-regexp
  (rx (: bos (* (not upper)) eos)))


;; Variables
(defvar rg3--append-persistent-buffers nil)

(defvar rg3--currently-opened-persistent-buffers nil)

(defvar rg3--current-overlays nil)

(defvar rg3--current-dir nil)

(defvar rg3--glob-string nil)

(defvar rg3--glob-string-history nil)


;; Logic
(defun rg3--make-dummy-process ()
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

(defun rg3--real-process-filter (proc ev)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert ev)))

(defun rg3--make-process ()
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
                         :filter #'rg3--real-process-filter
                         :noquery t))
             (helm-src-name
              (format "rg cmd: '%s' @ %s"
                      (mapconcat (lambda (s) (format "'%s'" s))
                                 rg-cmd " ")
                      rg3--current-dir)))
        (helm-attrset 'name helm-src-name)
        (set-process-query-on-exit-flag real-proc nil)
        real-proc))))

(defun rg3--ansi-color (line)
  (ansi-color-apply line))

(defun rg3--decompose-vimgrep-output-line (line)
  (-when-let* ((_ (string-match rg3--vimgrep-output-line-regexp line))
               ((file-path line-no col-no content)
                (--map (match-string it line) (number-sequence 1 4))))
    (list
     :file-path file-path
     :line-no (-> line-no string-to-number 1-)
     :col-no (-> col-no string-to-number 1-)
     :content content)))

(defun rg3--pcre-to-elisp-regexp (pcre)
  ;; This is very simple conversion
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
  (let ((olay (make-overlay beg end)))
    (overlay-put olay 'face face)
    olay))

(defun rg3--delete-overlays ()
  (cl-mapc #'delete-overlay rg3--current-overlays)
  (setq rg3--current-overlays nil))

(defun rg3--get-overlay-columns (elisp-regexp content)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward elisp-regexp nil t)
     for (beg end) = (match-data t)
     collect (list :beg (1- beg) :end (1- end)))))

(defun rg3--async-action (cand)
  (rg3--delete-overlays)
  (-if-let*
      (((&plist :file-path file-path
                :line-no line-no
                :col-no col-no
                :content content)
        (rg3--decompose-vimgrep-output-line cand))
       (file-abs-path
        (expand-file-name file-path rg3--current-dir))
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
      (progn
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
                 for (:beg beg :end end) in olay-cols
                 for pt-beg = (+ (point) beg)
                 for pt-end = (+ (point) end)
                 collect (rg3--make-overlay-with-face
                          pt-beg pt-end 'rg3-preview-match-highlight))))
          (setq rg3--current-overlays
                (append (list line-olay) match-olays)))
        (forward-char col-no)
        (recenter))
    (user-error "the line '%s' could not be parsed" cand)))

(defun rg3--async-persistent-action (cand)
  (let ((rg3--append-persistent-buffers t))
    (rg3--async-action cand)))

(defun rg3--kill-proc-if-live (proc-name)
  (let ((proc (get-process proc-name)))
    (when (process-live-p proc)
      (delete-process proc))))

(defun rg3--kill-bufs-if-live (&rest bufs)
  (cl-mapc
   (lambda (buf)
     (when (buffer-live-p (get-buffer buf))
       (kill-buffer buf)))
   bufs))

(defun rg3--unwind-cleanup ()
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
  (helm :sources '(rg3--process-source)
        :buffer rg3--helm-buffer-name
        :input rg-pattern
        :prompt "rg pattern: "))

(defun rg3--get-thing-at-pt ()
  (helm-aif (thing-at-point rg3-thing-at-point)
      (substring-no-properties it)
    ""))


;; Toggles and settings
(defmacro rg3--run-after-exit (&rest body)
  `(helm-run-after-exit (lambda () ,@body)))

(defun rg3--set-glob ()
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
  "???")


;; Helm sources
(defconst rg3--process-source
  (helm-build-async-source "ripgrep"
    :candidates-process #'rg3--make-process
    :candidate-number-limit rg3-candidate-limit
    :action (helm-make-actions "Visit" #'rg3--async-action)
    :filter-one-by-one #'rg3--ansi-color
    :persistent-action #'rg3--async-persistent-action
    :keymap 'rg3-map))


;; Autoloaded functions
;;;###autoload
(defun rg3 (rg-pattern)
  "???

\\{rg3-map}"
  (interactive (list (rg3--get-thing-at-pt)))
  (let* ((rg3--current-dir (or rg3--current-dir default-directory))
         (rg3--glob-string (or rg3--glob-string rg3-default-glob-string)))
    (unwind-protect (rg3--do-rg3 rg-pattern)
      (rg3--unwind-cleanup))))

(provide 'rg3)
;;; rg3.el ends here
