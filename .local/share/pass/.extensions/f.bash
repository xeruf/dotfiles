#!/bin/bash
set -e
# Decrypts all matching password store entries
case $PWD in ($PASSWORD_STORE_DIR/[a-z]*) dir=$PWD;; (*) dir=$PASSWORD_STORE_DIR;; esac
files="$(find -L "$dir" -name .extensions -prune -o -type f -path "*$1*" -printf '%p\n')"
test -n "$files" || exit 1
# TODO possible to cache passphrase without using an example file?
gpg --decrypt "$(echo "$files" | head -1)" >/dev/null
for file in $files; do
  path=${file//$PASSWORD_STORE_DIR\/}
  echo "[4m${path%.gpg}[0m"
  gpg --quiet --decrypt "$file"
done | ${PAGER:-less}
