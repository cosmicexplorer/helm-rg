#!/bin/bash

set -euxo pipefail

cargo build

emacs \
  --module-assertions \
  -Q \
  --batch \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))' \
  --eval '(message "%s" (helm-rg-string-match 1 2))'
