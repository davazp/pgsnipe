#!/bin/bash

SBCL=`which sbcl` || error "SBCL could not be found in the system."
$SBCL --non-interactive --load make.lisp
