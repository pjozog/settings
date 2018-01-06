#!/bin/sh

export GTK2_RC_FILES=/usr/share/themes/Radiance/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'
export QT_STYLE_OVERRIDE='gtk'

# fix for awesome and open-jdk version 7
export _JAVA_AWT_WM_NONREPARENTING=1

# force anti-aliased fonts
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

# source private env for non-volatile programs e.g. emacs
[ -f ${HOME}/Dropbox/code/private/private-shell-env ] && . ${HOME}/Dropbox/code/private/private-shell-env

# .Xmodmap should be sourced by lightdm on startup.  No need to put it here

# the idiots at Canonical took out --expire-time support for their desktop notification
# frontend.  Just use awesome's superior frontend instead.
killall notify-osd

which nvidia-settings > /dev/null 2>&1
[ $? -eq 0 ] && nvidia-settings --load-config-only &

which dropbox > /dev/null 2>&1
[ $? -eq 0 ] && dropbox start -i &

# Makes resizing work in vmware
which vmtoolsd > /dev/null 2>&1
[ $? -eq 0 ] && vmtoolsd -n vmusr &

nm-applet --sm-disable &

gnome-screensaver &

xset r rate 300 40

# disable uber annoying overlay scroll bar
export LIBOVERLAY_SCROLLBAR=0

# start emacs
emacs --daemon &

# for some reason this needs to be done last, otherwise it might not show up
which xfce4-power-manager > /dev/null 2>&1
[ $? -eq 0 ] && xfce4-power-manager &
