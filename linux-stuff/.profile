#!/bin/sh

#THIS FILE SHOULD CONTAIN ENV VARIABLES.  IT IS READ AT LOGIN.
#THE .ZSHRC/.BASHRC FILES SHOULD CONTAIN INTERACTIVE SETTINGS LIKE
#ALIASES, FUNCTIONS, ETC.

if [ $TERM = "Eterm" ]; then
    export LANG=C
fi

if [ -d $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
fi

if [ $TERM = "xterm" ]; then
    export TERM=xterm-256color
fi

export EDITOR='emacsclient'
export PYTHONSTARTUP=~/.pythonrc
export PYTHONPATH=${PYTHONPATH}:~/perls/python/lcmtypes

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:~/texpath:~/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:~/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:~/texpath:$BIBINPUTS

[ -d ${HOME}/lib64/python ] && export PYTHONPATH=$PYTHONPATH:~/lib64/python


which source-highlight > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
    export LESS=' -R'
fi

export PROFILE_SOURCED="TRUE"
