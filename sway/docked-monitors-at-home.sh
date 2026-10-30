#!/usr/bin/env bash

set -euo pipefail

# Left monitor
swaymsg output DP-6 mode 2560x1440 position 0 0

# Right monitor
swaymsg output DP-5 mode 2560x1440 position 2560 0

# Disable built-in display
swaymsg output eDP-1 disable
