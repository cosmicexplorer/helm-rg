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
  --eval '(message "%s" (helm-rg-string-match-p "df(" "asdf"))'


emacs \
  --module-assertions \
  -Q \
  --batch \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))' \
  --eval '(message "%s" (helm-rg-string-match-p "df" "asdf" -1))'
