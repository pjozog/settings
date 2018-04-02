#!/bin/sh

. ~/.config/autostart.sh

rm -f ${HOME}/.i3/config

cat ${HOME}/.i3/config.base >> ${HOME}/.i3/config

if [ -f ${HOME}/Dropbox/code/private/i3-private.sh ]; then
    ${HOME}/Dropbox/code/private/i3-private.sh >> ${HOME}/.i3/config
fi

# Inform X11 about terminus (a PCF font). Note that this doesn't mean apps like
# emacs will know about it; for that, refer to ~/.fonts.conf.
xset fp+ ${HOME}/.fonts/terminus-powerline

exec i3
