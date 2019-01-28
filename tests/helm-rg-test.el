;;; helm-rg-test.el --- Tests for helm-rg -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(require 'helm-rg)

(require 'cl-lib)
(require 'ert)

(defconst helm-rg-test-cwd
  (-> (or load-file-name buffer-file-name) (file-name-directory) (expand-file-name)))

(defconst helm-rg-test-time-step 0.2
  "Floating-point number of seconds to wait for steps in between interactive helm actions.")

(defun helm-rg-test--invoke-in-test-dir (pattern)
  (let ((default-directory helm-rg-test-cwd))
    (helm-rg pattern t)))

;;; TODO: this seems better done with a macro, but it's not clear how to reach into the
;;; `ert-deftest' forms to get to the top of the BODY forms.
(defun helm-rg-mark-interactive-test ()
  (when noninteractive
    (ert-skip "This test can unfortunately only be run interactively.")))

(ert-deftest test-helm-rg/resume ()
  "Test that the right file is visited when resuming a `helm-rg' session with `helm-resume'."
  (helm-rg-mark-interactive-test)
  (let ((debugger #'ignore))
    (run-at-time helm-rg-test-time-step nil (lambda () (helm-keyboard-quit)))
    (with-helm-quittable
      (helm-rg-test--invoke-in-test-dir "GPL"))
    (let ((helm-rg-buf helm-last-buffer))
      (sit-for helm-rg-test-time-step)
      (run-at-time helm-rg-test-time-step nil (lambda ()
                             (helm-exit-minibuffer)))
      (helm-resume helm-rg-buf)
      (should (equal (buffer-file-name) (concat helm-rg-test-cwd "LICENSE"))))))
