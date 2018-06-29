(require 'cl-lib)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(defconst dependent-packages '(dash helm))

(package-initialize)
(package-refresh-contents)

(cl-loop for pkg in dependent-packages
         do (package-install pkg))
