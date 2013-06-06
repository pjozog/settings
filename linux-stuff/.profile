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

export EDITOR='emacsclient -t'
export PYTHONSTARTUP=${HOME}/.pythonrc
export MATLABPATH=~/documents/MATLAB

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:${HOME}/texpath:${HOME}/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:${HOME}/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:${HOME}/texpath:$BIBINPUTS

[ -d ${HOME}/lib64/python ] && export PYTHONPATH=$PYTHONPATH:${HOME}/lib64/python


which source-highlight > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
fi
export LESS=' -R'

export PROFILE_SOURCED="TRUE"
