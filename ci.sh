#!/bin/bash

set -xe

emacs -batch -f package-initialize \
	  --eval '(add-to-list (quote package-archives) (quote ("melpa" . "http://melpa.org/packages/")))' \
	  --eval '(use-package alert :ensure t)'

cd test && make test
