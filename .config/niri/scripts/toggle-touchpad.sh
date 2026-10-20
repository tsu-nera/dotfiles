#!/usr/bin/env bash
# Toggle the touchpad on/off via the kernel `inhibited` sysfs attribute.
# This bypasses niri entirely, so it does NOT reload the config (no banner).
# Write access to `inhibited` is granted to the
# `wheel` group by /etc/udev/rules.d/99-touchpad-inhibit.rules.

set -euo pipefail

# Collect every input device that shares a parent with a "Touchpad" device.
# Some touchpads (iiyama's FTCS1000) move the cursor through a sibling
# "... Mouse" node, so inhibiting only the Touchpad node does nothing.
mapfile -t files < <(
    for n in /sys/class/input/input*/name; do
        grep -qi touchpad "$n" 2>/dev/null && readlink -f "$(dirname "$n")/device"
    done | sort -u | while read -r parent; do
        for d in /sys/class/input/input*; do
            [ "$(readlink -f "$d/device")" = "$parent" ] && [ -w "$d/inhibited" ] && echo "$d/inhibited"
        done
    done
)

if [ "${#files[@]}" -eq 0 ]; then
    notify-send -u critical "Touchpad toggle" "writable inhibited attribute not found"
    exit 1
fi

# Decide the new state from the first device, then apply it to all of them.
cur=$(cat "${files[0]}")
new=$([ "$cur" = "1" ] && echo 0 || echo 1)

for f in "${files[@]}"; do
    echo "$new" > "$f"
done

# Replace the previous popup so rapid presses don't stack, and so the current
# state is visible (silent toggling made it easy to undo by pressing twice).
notify-send -t 1500 -h string:x-canonical-private-synchronous:touchpad-toggle \
    "Touchpad $([ "$new" = "1" ] && echo OFF || echo ON)"
