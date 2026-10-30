#!/bin/sh

# this file should contain env variables.  it is read at login.  the
# .zshrc/.bashrc files should contain interactive settings like aliases,
# functions, etc.

export PATH=$HOME/.local/bin:$HOME/bin:$PATH

export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR='nano'

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:${HOME}/texpath:${HOME}/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:${HOME}/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:${HOME}/texpath:$BIBINPUTS

# Syntax highlighting in less
if command -v source-highlight >/dev/null 2>&1; then
    if [[ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]]; then
        export LESSOPEN='| /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
    elif [[ -f /usr/bin/src-hilite-lesspipe.sh ]]; then
        export LESSOPEN='| /usr/bin/src-hilite-lesspipe.sh %s'
    fi
fi

export LESS='-R'

# Private environment configuration
private_env="$HOME/ownCloud/code/private/private-shell-env"
if [[ -f "$private_env" ]]; then
    source "$private_env"
fi
