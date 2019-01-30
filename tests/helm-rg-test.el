;;; helm-rg-test.el --- Tests for helm-rg -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'helm-rg)

(require 'cl-lib)
(require 'ert)

(defconst helm-rg-test--cwd
  (-> (or load-file-name buffer-file-name) (file-name-directory) (expand-file-name)))

(defconst helm-rg-test--time-step 0.5
  "Floating-point number of seconds to wait for steps in between interactive helm actions.")

(defmacro helm-rg-test--delayed-do (sexp)
  "Evaluate SEXP after some time to simulate user input of some sort."
  `(run-at-time helm-rg-test--time-step nil (lambda () ,sexp)))

(defmacro helm-rg-test--define-interactive-test (name doc &rest body)
  "Define an `ert-deftest' wrapped to execute correctly when invoked from the command line."
  (declare (doc-string 1) (indent 1))
  `(ert-deftest ,name () ,doc
     (progn
       (helm-rg-test--mark-interactive)
       ;; We have to set the debugger to ignore because `ert' will abort the test after a keyboard
       ;; quit otherwise.
       (let ((debugger #'ignore)
             ;; We don't want dir-local variables (like the ones defined in this repo!) to be
             ;; activated in testing, so we set this to a file name which (hopefully!) doesn't
             ;; exist.
             (dir-locals-file "????????????????"))
         ,@body))))

(defun helm-rg-test--invoke-in-test-dir (pattern)
  "Invoke `helm-rg' in the tests directory with some input PATTERN."
  (let ((default-directory helm-rg-test--cwd))
    ;; NB: We could just add `helm-rg-test--cwd' to the PATHS here, but it feels like that might
    ;; miss the more common scenario of just running `helm-rg' in a directory.
    (helm-rg pattern t)))

;;; TODO: this seems better done with a macro, but it's not clear how to reach into the
;;; `ert-deftest' forms to get to the top of the BODY forms.
(defun helm-rg-test--mark-interactive ()
  (when noninteractive
    (ert-skip "This test can unfortunately only be run interactively.")))

(defun helm-rg-test--find-gpl ()
  "A function to find a string in a known file to test that actions can find the right file."
  (let ((helm-rg-default-glob-string "LICENSE"))
    (helm-rg-test--invoke-in-test-dir "GPL")))

(helm-rg-test--define-interactive-test test-helm-rg/helm-resume
  "Test that the right file is visited when resuming a `helm-rg' session with `helm-resume'."
  (helm-rg-test--delayed-do
   (helm-keyboard-quit))
  (with-helm-quittable
    (helm-rg-test--find-gpl))
  (let ((helm-rg-buf helm-last-buffer))
    (sit-for helm-rg-test--time-step)
    (helm-rg-test--delayed-do
     (helm-exit-minibuffer))
    (helm-resume helm-rg-buf)
    ;; The phrase "GPL" is expected to show up when searching the LICENSE file in the test
    ;; directory, and the `helm-exit-minibuffer' should select the file and visit it.
    (should (equal (buffer-file-name) (concat helm-rg-test--cwd "LICENSE")))))
