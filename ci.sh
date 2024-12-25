#!/bin/bash

set -xe

emacs -batch -f package-initialize \
	  --eval '(use-package alert :ensure t)'

cd test && make test
