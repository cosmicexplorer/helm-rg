(require 'cl-lib)

(package-initialize)

(defconst dependent-packages
  '(cl-lib
    dash
    helm))

(cl-loop for pkg in dependent-packages
         do (package-install pkg))
