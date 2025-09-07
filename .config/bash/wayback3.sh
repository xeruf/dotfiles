#!/bin/sh -e
# Recover website content from wayback machine 
# into an existing web folder of a customer.
# Execute with superuser permissions.
#
# Need to install wayback_machine_downloader gem first:
#   sudo apt install ruby
#   gem install wayback_machine_downloader_straw
#
# Args:
# 1. Domain
# 2. (optional) timestamp in the form YYYY[MM[DD[HH[MM]]]] to narrow down the snapshot time
# Any further parameters will be forwarded to the wayback_machine_downloader
#
# Note: Downloading a snapshot of a specific time will usually not work well,
#       as the wayback machine tends to scatter website content across multiple snapshots,
#       so I tend to use this script with the `--to` parameter and a year to restore old website versions.

domain="$1"
shift

target=$domain
if dir=$(find /home -maxdepth 3 -name "$domain" | grep web)
then
  echo "Found domain at $dir"
else
  echo "No existing user with domain found, enter a target path (default: $domain, abort with Ctrl-C)"
  read usertarget
  target="${usertarget:-$target}"
fi

case "$1" in
(""|-*) true;;
(*) args="--from $1 --to $1"; shift;;
esac
wayback_machine_downloader --all --concurrency 5 -d "$target" $args "$@" "$domain" 

if test -d "$dir"
then 
  html="$dir/public_html"
  test -e "$html" && case "$(cat $html/*.* | sha512sum)" in
  # Iridion default template as of writing this script - delete if matches
  (42561e2681ffb3c15867fc75b3cfe918d03e199cb0c234792eacee7e75a6a278d3ec9b7ce828a8ed1283d3de84a7fbc7333cd7ac4ed33f93979391a013d8a710*) mv -v "$html" "/tmp/$domain";; 
  # Otherwise move content aside
  (*) mv -v "$html" "$html.$(date +%FT%T)";;
  esac
  mv -v "$target" "$html"
  sudo chown -R $(echo "$dir" | cut -d/ -f3 | grep --max-count 1):www-data "$html"
fi

# Replace common URLs with their URL-encoded form to make them work statically
grep -rlI '?v' "$html" | xargs sed -i 's/?v=/%3Fv=/g;s/?ver=/%3Fver=/g'
