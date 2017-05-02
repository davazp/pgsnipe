#!/bin/bash
#
# run-pgsnipe.sh ---
#
# Execute pgsnipe command line tool without compiling. Useful for
# debugging the CLI. For general development you should load the ASDF
# system in your Lisp image instead.
#

set -e

SBCL=`which sbcl` || ( echo "ERROR: SBCL was not found" 1>&2; exit -1; )

$SBCL --noinform \
      --non-interactive \
      --eval "(handler-bind ((warning #'muffle-warning)) (asdf:load-system :pgsnipe-cli))" \
      --eval '(pgsnipe-cli:main)' \
      --end-toplevel-options \
      $@
