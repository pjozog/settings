#!/bin/sh
# Move the focused workspace to the next active output, cycling through all
# outputs ordered left-to-right (then top-to-bottom), wrapping around.

next=$(swaymsg -t get_outputs | jq -r '
  [.[] | select(.active)]
  | sort_by(.rect.x, .rect.y)
  | (map(.focused) | index(true)) as $i
  | if length < 2 or $i == null then empty else .[($i + 1) % length].name end
')

[ -n "$next" ] && swaymsg move workspace to output "$next"
