#!/bin/sh

. ~/.config/autostart.sh

rm -f ${HOME}/.i3/config

cat ${HOME}/.i3/config.base >> ${HOME}/.i3/config

if [ -f ${HOME}/Dropbox/code/private/i3-private.config ]; then
    cat ${HOME}/Dropbox/code/private/i3-private.config >> ${HOME}/.i3/config
fi

exec i3
