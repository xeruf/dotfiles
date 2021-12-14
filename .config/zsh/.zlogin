if test -z "$DISPLAY" && test "$XDG_VTNR" -eq 1 && test -e "$JOURNAL"; then
  echo "What do you want to do? Check your planner!"
  while test $(echo "$intention" | wc -c) -lt 6
  do read intention
  done
  jrnl intentions "$intention"
  exec startx
fi
