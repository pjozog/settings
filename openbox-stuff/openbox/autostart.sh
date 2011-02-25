#!/bin/bash

export GTK2_RC_FILES=/usr/share/themes/Radiance/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'

#Temporary fix for latex files being used in openbox
eval "$(grep 'export ...INPUTS=' ~/.bashrc)"

[ -f ${HOME}/.swapcaps ] && xmodmap .swapcaps
[ -f ${HOME}/swapcaps ] && xmodmap swapcaps
[ -f ${HOME}/.xmodmap  ] && xmodmap .xmodmap

rr

xmodmap -e "remove mod4 = grave"
xmodmap -e "remove mod1 = Alt_R"
xmodmap -e "add mod4 = Alt_R"

if [ -f ${HOME}/currentWallpaper ]; then
    Esetroot ${HOME}/currentWallpaper    
else
    Esetroot -s ~/pics/netwall/lava_mario.jpg
fi

gnome-power-manager &
gnome-volume-control-applet &
checkgmail &
nm-applet &

xscreensaver -no-splash &
