#!/bin/sh
set -eu

# Disable ACPI wakeup for USB controllers (internal keyboard + external USB wake laptop from suspend)
grep -E '^XHC[0-9]+.*\*enabled' /proc/acpi/wakeup | while read -r dev _; do
    echo "$dev" > /proc/acpi/wakeup
done

# Disable USB-C power-delivery wakeup sources (cable movement causes spurious PD renegotiation wakes)
for entry in /sys/class/wakeup/*/name; do
    name=$(cat "$entry")
    case "$name" in
        ucsi-source-psy-*|ACAD)
            dir=$(dirname "$entry")
            echo disabled > "$dir/device/power/wakeup" 2>/dev/null || true
            ;;
    esac
done
