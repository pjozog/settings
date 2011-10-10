#!/bin/sh

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

which nvidia-settings > /dev/null 2>&1
[ $? -eq 0 ] && nvidia-settings --load-config-only &

which dropbox > /dev/null 2>&1
[ $? -eq 0 ] && dropbox start -i &

which xcompmgr > /dev/null 2>&1
[ $? -eq 0 ] && xcompmgr -cf &

gnome-power-manager &
gnome-volume-control-applet &
nm-applet &

gnome-screensaver &
