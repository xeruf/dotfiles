set -o pipefail

alias localip="ip addr show | grep -E '(ens|eth)' | grep -oP '"'(?<=inet\s)\d+(\.\d+){3}'"' | head -1"

letsencrypt() {
  if test $# -eq 0
  then
    for user in $(list users)
    do letsencrypt "$user"
    done
  else
    (
      ip=$(localip)
      IFS=$'\n'
      for user
      do for domain in $(hestia v-list-web-domains $user | grep $ip | awk '{print $1}')
         do #echo commented out due to command echoing in hestia alias
          #echo "Checking $user $domain" >&2
          hestia v-list-web-domain $user $domain | grep -q REDIRECT && continue
          hestia v-list-mail-domain-ssl $user $domain | grep -q . || hestia v-add-letsencrypt-domain $user $domain '' yes
          hestia v-list-web-domain-ssl $user $domain | grep -q . && continue
          #echo "Generating Certificate" >&2
          hestia v-add-letsencrypt-domain $user $domain $(hestia v-list-web-domain $user $domain | grep ALIAS | tr -s ' ' | cut -d' ' -f2- | tr ' ' ',')
        done
      done
    )
  fi
}

list() {
  type="$1"
  shift
  if test $# -eq 0 && echo $type | grep -q -- '-domains$'
  then for user in $(list users)
       do if test -t 1
          then echo "[4m$user[0m" && list "$type" "$user"
          else list "$type" "$user" | sed "s|^|$user |"
          fi
       done
  else if test -t 1
         then sudo "$HESTIA/bin/v-list-$type" "$@" | column -t
         else sudo "$HESTIA/bin/v-list-$type" "$@" | tail +3 | awk '{print $1}'
       fi
  fi
}

hestia() {
  test $# -eq 0 && cd "$HESTIA" && return 0
  command=$1
  shift
  echo '>' sudo $(which $command) "$@" >&2
  sudo timeout 30s $(which $command) "$@"
}

accessible() {
  dir=/home/*/web/$1/public_html
  sudo chmod -v 755 $dir
  sudo chown -v :sudo $dir
}
