#!/bin/bash

set -euxo pipefail

emacs \
  --module-assertions \
  -Q \
  --eval '(module-load (expand-file-name "target/debug/libhelm_rg.so"))'
