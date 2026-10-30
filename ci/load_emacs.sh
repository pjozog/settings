#!/bin/sh
# Fully load init.el headless, installing the MELPA packages it wants, to prove
# the real Emacs config works end to end -- not just that it parses.
#
# init.el keeps `package-refresh-contents` commented out (it assumes a machine
# whose package cache already exists), so we refresh here before loading, then
# load the config and require that it reaches the end cleanly.
#
# MELPA is a network dependency, so we retry once to ride out a transient blip.
#
# Usage: load_emacs.sh [path/to/init.el]
set -eu

INIT="${1:-emacs-stuff/.emacs.d/init.el}"
INIT_ABS="$(cd "$(dirname "$INIT")" && pwd)/$(basename "$INIT")"

load_once() {
    emacs --batch \
        --eval '(setq debug-on-error t)' \
        --eval '(progn
                  (require (quote package))
                  (add-to-list (quote package-archives)
                               (quote ("melpa" . "https://melpa.org/packages/")) t)
                  (package-initialize)
                  (package-refresh-contents))' \
        -l "$INIT_ABS" \
        --eval '(message "=== INIT LOADED OK ===")'
}

if load_once; then
    exit 0
fi

echo ">>> first attempt failed; retrying once in case MELPA was flaky..." >&2
sleep 5
load_once
