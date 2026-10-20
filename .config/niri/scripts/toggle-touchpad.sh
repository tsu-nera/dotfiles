#!/usr/bin/env bash
# Toggle the touchpad on/off via the kernel `inhibited` sysfs attribute.
# This bypasses niri entirely, so it does NOT reload the config (no banner)
# and shows no notification. Write access to `inhibited` is granted to the
# `wheel` group by /etc/udev/rules.d/99-touchpad-inhibit.rules.

set -euo pipefail

# Collect every input device whose name contains "Touchpad".
mapfile -t files < <(
    for n in /sys/class/input/input*/name; do
        if grep -qi touchpad "$n" 2>/dev/null; then
            d=$(dirname "$n")
            [ -w "$d/inhibited" ] && echo "$d/inhibited"
        fi
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
