# Insert the contents of a file into pass or decrypt and open it
if test -f "$1"
  then
    cat "$1" | pass insert -m "${2:-$1}"
    mv -v "$1" /tmp/
    echo "Encrypted $1"
  else
    for file; do
      pass "$file" >"$(basename "$file")" && xdg-open "$file"
      # pass "${2:-$1}" >"$1" && xdg-open "$1"
      echo "Decrypted and opened $file"
    done
fi
