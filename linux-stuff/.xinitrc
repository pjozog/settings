#!/bin/bash

# If running ubuntu, make sure you're in the 'audio' group.  Otherwise, sound won't work

. ${HOME}/.bashrc
. ${HOME}/.config/autostart.sh

exec awesome
