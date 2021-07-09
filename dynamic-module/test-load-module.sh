#!/bin/bash

set -euxo pipefail

cargo build

emacs \
  --module-assertions \
  -Q \
  --batch \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))' \
  --eval '(message "%s" (helm-rg-string-match-p "df" "asdf"))'

set +e

emacs \
  --module-assertions \
  -Q \
  --batch \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))' \
  --eval '(message "%s" (condition-case e (helm-rg-string-match-p "df(" "asdf") (helm-rg-native-error (format "%s: %s" (nth 1 e) (nth 2 e)))))'


emacs \
  --module-assertions \
  -Q \
  --batch \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))' \
  --eval '(message "%s" (helm-rg-string-match-p "df" "asdf" -1))'
