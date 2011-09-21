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
export PYTHONPATH=${PYTHONPATH}:{$HOME}/perls/python/lcmtypes
export MATLABPATH=~/Documents/MATLAB

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:${HOME}/texpath:${HOME}/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:${HOME}/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:${HOME}/texpath:$BIBINPUTS

[ -d ${HOME}/lib64/python ] && export PYTHONPATH=$PYTHONPATH:${HOME}/lib64/python


which source-highlight > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
    export LESS=' -R'
fi

export PROFILE_SOURCED="TRUE"

#April codebase
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HOME}/Documents/umich-grad/EECS/f11/568/eecs568/java/jni
export CLASSPATH=${CLASSPATH}:.:${HOME}/Documents/umich-grad/EECS/f11/568/eecs568/java/april.jar
