#!/bin/sh

export GTK2_RC_FILES=/usr/share/themes/Radiance/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'

#Temporary fix for latex files being used in openbox
eval "$(grep 'export ...INPUTS=' ~/.bashrc)"

# .Xmodmap should be sourced by lightdm on startup.  No need to put it here

# xrandr
rr

if [ -f ${HOME}/currentWallpaper ]; then
    Esetroot -s ${HOME}/currentWallpaper    
else
    Esetroot -s ~/pics/netwall/lava_mario.jpg
fi

which nvidia-settings > /dev/null 2>&1
[ $? -eq 0 ] && nvidia-settings --load-config-only &

which dropbox > /dev/null 2>&1
[ $? -eq 0 ] && dropbox start -i &

which xcompmgr > /dev/null 2>&1
[ $? -eq 0 ] && xcompmgr -cf &

which xfce4-power-manager > /dev/null 2>&1
[ $? -eq 0 ] && xfce4-power-manager &

which ubuntuone-launch > /dev/null 2>&1
[ $? -eq 0 ] && ubuntuone-launch &

nm-applet --sm-disable &

gnome-screensaver &

xset r rate 300 40

# disable uber annoying overlay scroll bar
export LIBOVERLAY_SCROLLBAR=0
