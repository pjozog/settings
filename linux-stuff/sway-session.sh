#!/bin/bash

#
# This script introduces a mechanism to set environment vars before sway starts.
#
source "$HOME/.config/environment.sh"
exec /usr/bin/sway "$@"
