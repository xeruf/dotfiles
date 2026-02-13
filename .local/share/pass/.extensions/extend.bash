#!/bin/bash -e
# Create and enable a pass extension
extdir="$PASSWORD_STORE_DIR/.extensions"
if test $# -gt 0; then
  file="$extdir/$1.bash"
  test -f "$file" || printf "#!/bin/bash -e\n\n" >>"$file"
  chmod +x "$file"
  $EDITOR $(test "$EDITOR" = "nvim" && echo "+") "$file"
else
  cd "$extdir"
  ls --color 
  exec $SHELL
fi
