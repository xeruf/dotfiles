if test -z "$DISPLAY" && test "$XDG_VTNR" -eq 1 && which startx >/dev/null
then exec startx
fi
