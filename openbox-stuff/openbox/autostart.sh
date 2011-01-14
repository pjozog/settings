#!/bin/bash

export GTK2_RC_FILES=/usr/share/themes/Radiance/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'

#Temporary fix for latex files being used in openbox
eval "$(grep 'export ...INPUTS=' ~/.bashrc)"

[ -f ${HOME}/.swapcaps ] && xmodmap .swapcaps
[ -f ${HOME}/swapcaps ] && xmodmap swapcaps
[ -f ${HOME}/.xmodmap  ] && xmodmap .xmodmap

gnome-keyring-daemon

rr

xrandr -q | grep -q 'VGA1 connected' && Esetroot ~/pics/netwall/lava_mario_dual.jpg || Esetroot -s ~/pics/netwall/lava_mario.jpg

gnome-power-manager &
gnome-volume-control-applet &
gnome-panel &
checkgmail &
nm-applet &
