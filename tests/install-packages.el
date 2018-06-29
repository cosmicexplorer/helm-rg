(require 'cl-lib)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.milkbox.net/packages/")))

(package-initialize)

(defconst dependent-packages '(dash helm))

(cl-loop for pkg in dependent-packages
         do (package-install pkg))
