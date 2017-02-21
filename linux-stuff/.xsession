#!/bin/sh

. ~/.config/autostart.sh

rm -f ${HOME}/.i3/config

cat ${HOME}/.i3/config.base >> ${HOME}/.i3/config

if [ -f ${HOME}/Dropbox/code/private/i3-private.sh ]; then
    ${HOME}/Dropbox/code/private/i3-private.sh >> ${HOME}/.i3/config
fi

exec i3
