(require 'cl-lib)

(package-initialize)

(defconst dependent-packages
  '(dash
    helm))

(cl-loop for pkg in dependent-packages
         do (package-install pkg))
