#!/bin/sh

export GTK2_RC_FILES=/usr/share/themes/Adwaita/gtk-2.0/gtkrc:${HOME}/.gtkrc-mine
export OOO_FORCE_DESKTOP='gnome'
export QT_STYLE_OVERRIDE='gtk2'
# On Ubuntu 18.04, apt install qt5-style-plugins to make this work.
export QT_QPA_PLATFORMTHEME='gtk2'

# force anti-aliased fonts
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

# source private env for non-volatile programs e.g. emacs
[ -f ${HOME}/ownCloud/code/private/private-shell-env ] && . ${HOME}/ownCloud/code/private/private-shell-env

which nvidia-settings > /dev/null 2>&1
[ $? -eq 0 ] && nvidia-settings --load-config-only &

which owncloud > /dev/null 2>&1
[ $? -eq 0 ] && owncloud &

# Makes resizing work in vmware.
which vmtoolsd > /dev/null 2>&1
[ $? -eq 0 ] && vmtoolsd -n vmusr &

nm-applet --sm-disable &

gnome-screensaver &

xset r rate 300 40

# Disable uber annoying overlay scroll bar.
export LIBOVERLAY_SCROLLBAR=0

emacs --daemon &

# for some reason this needs to be done last, otherwise it might not show up
which xfce4-power-manager > /dev/null 2>&1
[ $? -eq 0 ] && xfce4-power-manager &

# Overwite any existing i3 config.
cat ${HOME}/.i3/config.base > ${HOME}/.i3/config

if [ -f ${HOME}/ownCloud/code/private/i3-private.sh ]; then
    ${HOME}/ownCloud/code/private/i3-private.sh >> ${HOME}/.i3/config
fi

# Inform X11 about terminus (a PCF font). Note that this doesn't mean apps like
# emacs will know about it; for that, refer to ~/.config/fontconfig/fonts.conf.
xset +fp ${HOME}/.fonts/terminus-powerline

exec i3
