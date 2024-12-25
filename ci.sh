#!/bin/bash

set -xe

emacs -batch -f package-initialize \
	  --eval '(use-package :ensure t alert)'

cd test && make test
