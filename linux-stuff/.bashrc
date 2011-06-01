# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
if [ $TERM = "Eterm" ]; then
    export LANG=C
fi

if [ -d $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
fi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=50000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f $HOME/.svn_project ]; then
    . $HOME/.svn_project
fi

#only list directories first if ls supports it
ls --group-directories-first > /dev/null 2>&1
if [ $? -eq 0 ]; then
    alias ls='ls --color=always -CF --group-directories-first'
else 
    alias ls='ls --color=always -CF'
fi

# function pu {
#     arg="$1"
#     fullname=$(readlink -f "$arg")
#     match=$(dirs -v | sed 's@~@'${HOME}'@' | egrep "[0-9] +$fullname"'$')
#     if [ $? -eq 0 ]; then
# 	num=$(echo $match | awk '{print $1}')
# 	echo $num
# 	pushd +$num > /dev/null 2>&1
#     else
# 	pushd $arg > /dev/null 2>&1
#     fi
#     unset arg match fullname num
# }

alias l='ls'
alias ll='ls -lh'
alias lt='ll -tr'
alias ds='du --max-depth=1 -h'
alias g='grep --color=auto'
alias gg='grep --color=always'
alias eg='egrep --color=auto'
alias pu='pushd' #see above function
alias po='popd'
alias d='dirs -v'
alias fm='nautilus --no-desktop "$(pwd)"'
alias less='less -i'
alias e='emacsclient -n'
alias snes9x='snes9x -conf ${HOME}/.snes96_snapshots/snes9x.conf'
alias u='df -h .'
alias mountthumb='sudo mount /dev/sdc1 /mnt/thumb -o uid=1000,gid=1000'
alias mountsvn='sudo mount -t ext3 /dev/sdc1 /mnt/svn'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

# subversion aliases
alias sd='svn diff --diff-cmd meld'
alias sdd='svn diff'
function sl() {
    svn log -v "$1" | less
}
alias ss='svn status'
alias hd='hg extdiff -p meld'
function hl() {
    hg glog -v "$1" | less
}

function hs() {
    if [ $# -eq 1 ]; then
	hg status "$1"
    else
	hg status .
    fi
}

alias ns='ssh -X -p 2219 pjozog@nslab.ccs.neu.edu'

alias rmq='rm -r $(svn status | grep ^? | awk "{print \$2}")'
alias hrmq='rm -r $(hs | grep ^? | awk "{print \$2}")'
alias get='sudo apt-get install'
alias rs='rsync --stats --progress -avz'

function k() {
    arg="$1"
    processToKill=$(ps aux | grep "$arg" | awk '{print $2}')
    kill -9 $processToKill > /dev/null 2>&1
    unset processToKill arg
}

#usenet .nzb function
HELLANZBDIR=${HOME}/external/.hellanzb
function mz() {
    if [ ! -d ${HELLANZBDIR}/nzb/daemon.queue ]; then
	mkdir -p ${HELLANZBDIR}/nzb/daemon.queue
    fi
    mv "$1" ${HELLANZBDIR}/nzb/daemon.queue
}

[ -f $HOME/.dircolors ] && eval $(dircolors -b $HOME/.dircolors)

#green prompt, turquosie path
#export PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ '

#yellow prompt, turquosie path
export PS1='\[\033[01;33m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ '

[ ${HOSTNAME} = "perl-paulozog" ] && export PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;33m\]\w\[\033[00m\]\$ '

which source-highlight > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
    export LESS=' -R'
fi

export EDITOR='emacsclient'
export PYTHONSTARTUP=~/.pythonrc

[ -d ${HOME}/texpath ] && export TEXINPUTS=.:~/texpath:~/texpath/images:$TEXINPUTS
[ -d ${HOME}/texpath ] && export BSTINPUTS=.:~/texpath:$BSTINPUTS
[ -d ${HOME}/texpath ] && export BIBINPUTS=.:~/texpath:$BIBINPUTS

[ -d ${HOME}/lib64/python ] && export PYTHONPATH=$PYTHONPATH:~/lib64/python

alias m='wmname LG3D; matlab'
alias psy='wmname LG3D; cdp; ./perls-spy'

[ -f /opt/ros/diamondback/setup.bash ] && . /opt/ros/diamondback/setup.bash

which mycowsay > /dev/null 2>&1
[ $? -eq 0 ] && mycowsay
