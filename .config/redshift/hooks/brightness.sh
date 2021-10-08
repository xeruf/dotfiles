#!/bin/sh
# Set brightness via xbrightness when redshift status changes
# https://wiki.archlinux.org/title/Redshift#Use_real_screen_brightness

# Set brightness values for each status.
# Range from 1 to 100 is valid
brightness_day=100
brightness_transition=90
brightness_night=80
# Set fps for smoooooth transition
fps=1000
# Adjust this grep to filter only the backlights you want to adjust
backlights=($(xbacklight -list | grep ddcci*))

set_brightness() {
	xbacklight -set $1 -fps $fps &
}

if [ "$1" = period-changed ]; then
	case $3 in
		night)
			set_brightness $brightness_night
			;;
		transition)
			set_brightness $brightness_transition
			;;
		daytime)
			set_brightness $brightness_day
			;;
	esac
fi
