#!/bin/sh

export GTK2_RC_FILES=/usr/share/themes/Adwaita/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'
export QT_STYLE_OVERRIDE='gtk2'
# On Ubuntu 18.04, apt install qt5-style-plugins to make this work.
export QT_QPA_PLATFORMTHEME='gtk2'

# Fix for awesome and open-jdk version 7.
export _JAVA_AWT_WM_NONREPARENTING=1

# Force anti-aliased fonts.
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

# Source private env for non-volatile programs e.g. emacs.
[ -f ${HOME}/ownCloud/code/private/private-shell-env ] && . ${HOME}/ownCloud/code/private/private-shell-env

which nvidia-settings > /dev/null 2>&1
[ $? -eq 0 ] && nvidia-settings --load-config-only &

which owncloud > /dev/null 2>&1
[ $? -eq 0 ] && owncloud &

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

# Note this overwrites the .i3/config that was there previously.
cat ${HOME}/.i3/config.base > ${HOME}/.i3/config

# Append private config.
if [ -f ${HOME}/ownCloud/code/private/i3-private.sh ]; then
    ${HOME}/ownCloud/code/private/i3-private.sh >> ${HOME}/.i3/config
fi

# Inform X11 about terminus (a PCF font). Note that this doesn't mean apps like
# emacs will know about it; for that, refer to ~/.config/fontconfig/fonts.conf.
xset +fp ${HOME}/.fonts/terminus-powerline

exec i3
