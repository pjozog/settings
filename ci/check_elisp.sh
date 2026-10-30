#!/bin/sh
# Syntactically validate Emacs Lisp files with `check-parens`, which flags
# unbalanced parens/quotes without evaluating the code. We deliberately do NOT
# fully load init.el: it installs packages from MELPA at startup, so a real
# load needs the network and dozens of third-party packages. check-parens
# catches the mistakes that actually brick a config (a stray paren) and is
# hermetic.
#
# Usage: check_elisp.sh <file.el> [<file.el> ...]
set -e
status=0
for f in "$@"; do
    if emacs --batch --eval "(progn (find-file \"$f\") (check-parens))" \
            >/dev/null 2>/tmp/check_elisp.err; then
        echo "OK  $f"
    else
        echo "ERR $f"
        cat /tmp/check_elisp.err
        status=1
    fi
done
exit "$status"
