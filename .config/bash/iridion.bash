set -o pipefail

alias localip="ip addr show | grep -E '(ens|eth)' | grep -oP '"'(?<=inet\s)\d+(\.\d+){3}'"' | head -1"
ip=`localip`

logs() {
  if test $# -eq 0
  then
    lnav $(pwd | cut -d '/' -f1-5)/{logs,document_errors}
  else
    for site in $(find /home -maxdepth 3 -name "*$1*")
    do cd $site
       lnav logs
    done
  fi
}

monitor() {
  file=gatus.yaml
  echo 'customer-endpoint: &customer
  interval: 10m
  conditions:
    - "[STATUS] == 200"
endpoints:' >$file
  for user in $(list users)
  do group="$(hestia v-list-user $user | head -3 | cut -d':' -f2 | tr -s ' ' | sed 'N;s/\n/:/;N;s/\n / (/;s/$/)/')"
    for domain in $(hestia v-list-web-domains $user | grep "$ip" | awk '{print $1}')
    do echo '- name: "'$domain'"
  <<: *customer
  group: "'$group'"
  url: "https://'$domain'"' >>$file
    done
    for domain in $(hestia v-list-mail-domains $user | tail +3 | awk '{print $1}')
    do echo '- name: "Mail '$domain'"
  <<: *customer
  group: "'$group'"
  url: "https://mail.'$domain'"' >>$file
    done
  done
}

letsencrypt() {
  if test $# -eq 0
  then
    for user in $(list users)
    do letsencrypt "$user"
    done
  else
    (
      IFS=$'\n'
      for user
      do
        for domain in $(hestia v-list-dns-domains $user | tail +3 | awk '{print $1}')
        do hestia v-add-remote-dns-domain $user $domain
        done
        for domain in $(hestia v-list-mail-domains $user | tail +3 | awk '{print $1}')
        do hestia v-list-mail-domain-ssl $user $domain | grep -q . || hestia v-add-letsencrypt-domain $user $domain '' yes
        done
        for domain in $(hestia v-list-web-domains $user | grep "$ip" | awk '{print $1}')
        do #echo commented out due to command echoing in hestia alias
          #echo "Checking $user $domain" >&2
          hestia v-list-web-domain $user $domain | grep -q REDIRECT && continue
          #hestia v-list-mail-domain-ssl $user $domain | grep -q . || hestia v-add-letsencrypt-domain $user $domain '' yes
          hestia v-list-web-domain-ssl $user $domain | grep . >/dev/null && continue
          #echo "Generating Certificate" >&2
          hestia v-add-letsencrypt-domain $user $domain $(hestia v-list-web-domain $user $domain | grep ALIAS | tr -s ' ' | cut -d' ' -f2- | tr ' ' ',')
          echo "$domain: ${?}"
        done
        echo "Waiting an hour to not trigger letsencrypt rate limits..."
        time=0
        while test $time -lt 2000
        do sleep 100
           sudo -v
           time=$((time + 100))
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
         else sudo "$HESTIA/bin/v-list-$type" "$@" | tail +3 | $(if test $type = users; then echo "grep -v yes"; else echo cat; fi) | awk '{print $1}'
       fi
  fi
}

hestia() {
  test $# -eq 0 && cd "$HESTIA" && return 0
  test "$1" = "-x" && shift && set -x
  command=$1
  shift
  echo '>' sudo "$(which $command)" "$@" >&2
  export SHELLOPTS
  sudo --preserve-env=SHELLOPTS timeout 30s $(which $command) "$@"
  set +x
}

accessible() {
  dir=/home/*/web/$1/public_html
  sudo chmod -v 755 $dir
  sudo chown -v :sudo $dir
}
