#!/bin/bash

# This file should contain env variables. It is read at login, if using
# sway-session.sh.

source ~/.profile

eval $(ssh-agent)
ssh-add ${HOME}/.ssh/keys/id_rsa-*-dev

export GTK2_RC_FILES=${HOME}/.gtkrc-mine
export QT_STYLE_OVERRIDE='kvantum'
