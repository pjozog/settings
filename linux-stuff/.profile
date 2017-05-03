#!/bin/sh

# this file should contain env variables.  it is read at login.  the
# .zshrc/.bashrc files should contain interactive settings like aliases,
# functions, etc.

if [ $TERM = "Eterm" ]; then
    export LANG=C
fi

if [ -d $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
fi

export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR='nano'

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:${HOME}/texpath:${HOME}/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:${HOME}/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:${HOME}/texpath:$BIBINPUTS

which source-highlight > /dev/null 2>&1
if [ $? -eq 0 ]; then
    if [ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
        export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
    else
        export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
    fi
fi
export LESS=' -R'

export PROFILE_SOURCED="TRUE"
