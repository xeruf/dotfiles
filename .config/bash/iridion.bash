set -o pipefail

alias localip="ip addr show | grep -E '(ens|eth)' | grep -oP '"'(?<=inet\s)\d+(\.\d+){3}'"' | head -1"
test "$PS1" && IP=$(localip)

goto() {
  local dir=$(sudo find /home -maxdepth 3 -name "$1*" | fzf -0 -1)
  cd "$dir"
}

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

hestia() {
  test $# -eq 0 && cd "$HESTIA" && return 0
  test "$1" = "-x" && shift && set -x
  command=$1
  shift
  echo '>' sudo "$(which $command)" "$@" >&2
  export SHELLOPTS
  sudo --preserve-env=SHELLOPTS timeout 30s $(which $command) "$@"
  res=$?
  set +x
  return $res
}

make_accessible() {
  dir=/home/*/web/$1/public_html
  sudo chmod -v 755 $dir
  sudo chown -v :sudo $dir
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
    for domain in $(hestia v-list-web-domains $user | grep "$IP" | awk '{print $1}')
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
        for domain in $(hestia v-list-web-domains $user | grep "$IP" | awk '{print $1}')
        do #echo commented out due to command echoing in hestia alias
          #echo "Checking $user $domain" >&2
          hestia v-list-web-domain $user $domain | grep -q REDIRECT && continue
          #hestia v-list-mail-domain-ssl $user $domain | grep -q . || hestia v-add-letsencrypt-domain $user $domain '' yes
          hestia v-list-web-domain-ssl $user $domain | grep . >/dev/null && continue
          #echo "Generating Certificate" >&2
          hestia v-add-letsencrypt-domain $user $domain $(hestia v-list-web-domain $user $domain | grep ALIAS | tr -s ' ' | cut -d' ' -f2- | tr ' ' ',')
          echo "$domain status code: ${?}"
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


## BILLING scripts for InvoiceNinja

invoice() {
  local prefixes='^(d|dp|xe)'
  if test $# -eq 0
  then
    for user in $(list users | grep -E "${prefixes}[0-9]")
    do userdomains "$user"
       invoice "$user"
       echo
    done
    return $?
  fi
  
  local userid="$1"
  shift

  local userinfo
  for username in "$userid" "dp$userid" "xe$userid"
  do userinfo="$(hestia v-list-user "$username")" && break
  done
  local package=$(echo "$userinfo" | grep PACKAGE | cut -d: -f2)
  local name=$(echo "$userinfo" | grep FULL | cut -d: -f2)

  echo "Client Name,Invoice Number,Item Quantity,Item Cost,Item Notes,Item Product,Item Discount"
  local prefix="$name,DR$(date +%y%m)-$userid,"
  for domain
  do invoiceline "$domain" "$prefix"
  done
  for domain in $(list dns-domains "$username")
  do invoiceline "$domain" "$prefix"
  done
  echo "${prefix}12,9,Januar 2025 - Dezember 2025,Webpaket $package,20"
  # TODO recurring, batch
}

increment_day() {
  awk -F. '{print $3"-"$2"-"$1}' | xargs -I{} date -d "{} +1 day" "+%d.%m.%Y"
}

invoiceline() {
  local domain=$1
  local prefix=$2

  date=$(grep "$domain (" cu_invoicelineitems.csv | grep -v SSL | tail -1 | cut -d\" -f8 | sed -sE 's/.* - (.*)\).*/\1/' | increment_day)
  datec=$(grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f3 | cut -dT -f1)
  renew=$(grep ",$domain," domains.csv | cut -d, -f10 || grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f6 | cut -dT -f1)
  
  echo "${prefix}2,7,$domain (${date:-${datec:-$(date '+%d.%m.%Y')}} - $(date -d "${renew:-+1 year} -1 day" '+%d.%m.%Y')),.de-Domain,0"
}

domain() {
  if test $# -eq 0
  then while read -r domain
       do test -n "$domain" || break
          domain "$domain"
       done
       return $?
  fi

  local color
  case "$1" in (--color) color=true; shift;; esac

  for domain; do
    domain=$(echo "$domain" | rev | cut -d. -f-2 | rev)

    date=$(grep "$domain (" cu_invoicelineitems.csv | grep -v SSL | tail -1 | cut -d\" -f8 | sed -sE 's/.* - (.*)\).*/\1/')
    datec=$(grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f3 | cut -dT -f1)
    contact=$(grep ",$domain," domains.csv | cut -d, -f3 || grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f25)
    renew=$(grep ",$domain," domains.csv | cut -d, -f10 || grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f6 | cut -dT -f1)
    iddomain="$(idn2 "$domain")"
    dns="$(timeout .3s dig +short NS "$iddomain" | sort | cut -c-15 | head -1)"
    dnsa="$(timeout .3s dig +short A "$iddomain" | head -1)"
    case "$dns" in
      ("") style=3;;
      ("ns1.iridion.it.") style=0;;
      (*) style=2;;
    esac
    if test "$color" || test -t 1
    then styling="[${style}m [0m"
    fi
    printf "%s${dns:-	}	${dnsa:-	}	${date:-C$datec}	${renew:-	}	$domain	$contact%s\n" $styling
    #echo "[${style}m${dns:-	}	$(timeout .3s dig +short A "$iddomain" | head -1 | grep . || echo "	")	${date:-C$datec}	${renew:-	}	$domain	$contact[0m"
  done
}

userdomains() {
  user="$1"
  shift
  #CONTACT: local -A contacts=()
  name="$("$HESTIA/bin/v-list-user" $(echo "$user" | cut -d\  -f1) | grep 'FULL NAME:' | cut -d: -f2)"
  if test -t 1 # Avoid special symbols if not a terminal
  then echo "$user" | awk '{print "[4m"$3"	"$1"	'"$(echo $name)"'[0m"}'
  else echo "$user" | awk '{print "\n"$3"	"$1"	'"$(echo $name)"'"}' | tr '[a-z]' '[A-Z]'
  fi

  local color=""
  if test $# -gt 0
  then
    for domain
    do test "$domain" && domain "$domain"
    done
    color="--color"
  fi

  outfile="/tmp/${user}-domains.csv"
  sudo "$HESTIA/bin/v-list-dns-domains" $(echo "$user" | cut -d\  -f1) | tail +3 | sort | while read domain
    do  domain="${domain%% *}"
        if test $(echo "$domain" | tr -cd '.' | wc -c) -ne 1
        then #echo "Ignoring invalid DNS-Domain $domain" >&2
             continue
        fi
        domain $color "$domain"
        #CONTACT: contact=$(grep ",$domain," domains.csv | cut -d, -f3 || grep ",$domain," portfolio_domains_2025-01-13.csv | cut -d, -f25)
        #CONTACT: if test -n "$contact" && [[ -z "${contacts["$contact"]}" ]]
        #CONTACT: then contacts["$contact"]="$contact"
        #CONTACT: fi
    done | tee "$outfile"
  #CONTACT: for contact in "${!contacts[@]}"; do echo "C $contact"; done

  if test $# -eq 0 
  then
    contacts="$outfile-contacts"
    cat "$outfile" | rev | cut -d\	 -f1 | rev | sort | uniq | grep . > "$contacts"
    test -s "$contacts" || return
    echo
    echo Domains for $(cat "$contacts")
    domains --file "$contacts"
  fi
}

# Domains of all users plus list from all providers as of last export
# If arg is provided, only from that domain contact
domains() {
  if test $# -gt 0 
  then grep --no-filename "$@" ./portfolio_domains_2025-01-13.csv ./domains.csv |
         cut -d, -f$(case "$1" in (-*) echo '2-3';; (*) echo '1-3,25';; esac) |
         sort | column -s, -t
       return $?
  fi
  
  set -o pipefail
  sudo "$HESTIA/bin/v-list-users" | tail +3 | grep -v ssh- |
    while read user; do userdomains "$user"; done

  echo
  if test -t 1
  then echo "[4mVAUTRON[0m"
  else echo "VAUTRON"
  fi
  for domain in $(cut -d, -f2 domains.csv)
  do domain "$domain"
  done

  echo
  if test -t 1
  then echo "[4mAUTODNS[0m"
  else echo "AUTODNS"
  fi
  for domain in $(cut -d, -f2 portfolio_domains_2025-01-13.csv)
  do domain "$domain"
  done
}
