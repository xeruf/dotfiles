set -o pipefail

alias localip="ip addr show | grep -E '(ens|eth)' | grep -oP '"'(?<=inet\s)\d+(\.\d+){3}'"' | head -1"
test "$PS1" && IP=$(localip)

goto() {
  local dir=$(sudo find /home -maxdepth 3 -name "$1*" | fzf -0 -1)
  cd "$dir"
}

# logs for specific domain or current dir
logs() {
  if test $# -eq 0
  then
    lnav $(pwd | cut -d '/' -f1-5)/{logs,document_errors} ||
      echo 'Provide domain name to find logs!' >&2
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
  then # list domains for each user if no user specified
    for user in $(list users)
    do if test -t 1
       then echo "[4m$user[0m" && list "$type" "$user"
       else list "$type" "$user" | sed "s|^|$user |"
       fi
    done
  else
    case "$type" in
      (*s) if test -t 1
             then sudo "$HESTIA/bin/v-list-$type" "$@" | column -t
             else sudo "$HESTIA/bin/v-list-$type" "$@" | tail +3 |
                 $(if test "$type" = users; then echo "grep -v yes"; else echo cat; fi) | # only list non-suspended users
                 awk '{print $1}'
           fi;;
      (*) sudo "$HESTIA/bin/v-list-$type" "$@";;
    esac
  fi
}


## BILLING scripts for InvoiceNinja

vautron_csv=./domains.csv
# export with no headers, comma as separator from https://cloud.autodns.com/portfolio/domains/
autodns_csv=./autodns-domains.csv

# Invoice multiple Iridion customers
invoices() {
  if test $# -eq 0
  then
    echo " <> Invoice information for all customers <>"
    echo
    local prefixes='^(d|dp|xe|ir)'
    for user in $(list users | grep -E "${prefixes}[0-9]")
    do userdomains "$user"
       echo
       #invoice "$user"
       #echo
    done
    echo
    echo " <> ALL INVOICES <>"
    invoice ""
    for user in $(list users | grep -E "${prefixes}[0-9]")
    do invoice "$user" | tail +2
    done
    echo
    echo " <> RECURRING INVOICES <>"
    invoice ""
    for user in $(list users | grep -E "${prefixes}[0-9]")
    do invoice DR "$user" | tail +2
    done
    return $?
  else
    invoice ""
    for user
    do invoice "$user" | tail +2
    done
    echo
    for user
    do invoice DR "$user" | tail +2
    done
    #for user
    #do userdomains "$user"
    #done
  fi
}

# Invoice for an Iridion customer, optionally with extra domains
invoice() {
  if test $# -eq 0
  then echo "Usage: ${FUNCNAME[0]} CUSTOMER [DOMAINS...]" >&2
       return 1
  fi
  
  local years
  case "$1" in
  (-1|[0-9])
    years=$1
    shift
    ;;
  (DR)
    local numprefix=DR
    years=1
    shift
    ;;
  esac

  local userid="$1"
  shift

  local userinfo username
  if test "$userid"; then
    for username in "$userid" "dp$userid" "xe$userid"
    do userinfo="$("$HESTIA/bin/v-list-user" "$username")" && break
    done
    local name=$(echo "$userinfo" | grep FULL | cut -d: -f2 | sed 's/^ *//;s/ *$//')
  fi

  # TODO is amount discount not working so far
  echo "Client Name;Invoice Number;Invoice Status;Invoice Is Amount Discount;Item Is Amount Discount;Invoice Tax Name 1;Invoice Tax Rate 1;Item Cost;Item Product;Item Notes;Item Quantity;Item Discount"
  # echo "Kunde - Name;Rechnung - Nummer;Artikel - Menge;Artikel - Kosten;Artikel - Notizen;Artikel - Rabatt"
  local prefix="$name;${numprefix:-D}$(date +%y%m)-$userid;Draft;False;False;Ust.;19,00;"
  {
  for domain
  do echo "$prefix$(invoicedomain "$domain" $years)"
  done
  for domain in $(list dns-domains "$username")
  do test $(echo "$domain" | tr -cd '.' | wc -c) -ne 1 ||
      echo "$prefix$(invoicedomain "$domain" $years)"
  done
  } | $(test "$numprefix" && echo "sed s/202[1-5]/:YEAR$(case "$years" in (-*) echo "$years";; esac)/;s/202[2-6]/:YEAR+1/" || echo cat)

  local package=$(echo "$userinfo" | grep PACKAGE | cut -d: -f2 | sed 's/^ *//')
  if test -n "$package"
  then
    local price
    case "$package" in
      (alpha) price=4,5;;
      (beta) price=9;;
      (gamma) price=18;;
      (delta) price=29;;
    esac
    local year=$(test "$numprefix" && echo ':YEAR' || date +%Y)
    echo "${prefix}${price};Webpaket $package;Januar $year - Dezember $year;12;20"
    # echo "${prefix}${price};Webpaket $package;Januar 2023 - Dezember 2024;24;20"
  fi
}

increment_day() {
  awk -F. '{print $3"-"$2"-"$1}' | xargs -I{} date -d "{} +1 day" "+%d.%m.%Y"
}

# Create an invoiceline for a domain based on registrar data, tld and years
invoicedomain() {
  (
  set -eo pipefail
  local domain=$1
  shift
  test -n "$domain" || return 1

  local date_billed=$(grep "$domain (" cu_invoicelineitems.csv | grep -v SSL | tail -1 | cut -d\" -f8 | sed -sE 's/.* - (.*)\).*/\1/' | increment_day)
  local renew=$(grep ",$domain," "${vautron_csv}" | cut -d, -f10 || grep ",$domain," "${autodns_csv}" | cut -d, -f6 | cut -dT -f1)
  local date_created=$(grep ",$domain," "${autodns_csv}" | cut -d, -f3 | cut -dT -f1 || date -d "$renew -1 year" "+%Y-%m-%d")
  local date_created_de=$(echo "$date_created" | awk -F- '{print $3 "." $2 "." $1}')
  local date_start=$(test "$date_billed" && { echo "$date_billed" | awk -F. '{print $3 "-" $2 "-" $1}' ;} || echo "$date_created")
  local years=$(expr 1 \& "${1:-2}" \< 2 \| "${1:-5}" - "$(echo "$date_start" | cut -c4)")

  local date_billed_fut=$(date -d "$date_start +$years year -1 day" '+%d.%m.%Y')
  local renew_fut=$(date -d "${renew:-+1 year} -1 day" '+%d.%m.%Y')
  local date_fut="${date_billed_fut}$( test -n "$renew" && test -n "$date_start" && (( diff = 10#${renew:5:2} - 10#${date_start:5:2}, diff > -2 || diff < 2 )) || echo " [${renew_fut}]")"
  
  local price
  local suffix=${domain#*.}
  case "$suffix" in
    (de) price='7,85';;
    (eu) price='13,85';; # from 5€
    (name|org|com|at|ch|us) price='19,85';;
    (me|nl|nexus|net|it|cc) price='28,82';; # from 13€
    (blog|info) price='48,84';; # from 22€
    (tv|space|house|group) price='58,85';; # from 30€
    (online|digital) price='68,86';; # from 39€
    (gmbh) price='78,87';; # from 50€
    (codes|coach) price='98,89';; # from 50€
  esac

  echo "$price;.$suffix-Domain;$domain (${date_billed:-${date_created_de:-$(date '+%d.%m.%Y')}} - ${date_fut});$years;0"
  )
}

# Find info on a domain from registrars
domaininfo() {
  if test $# -eq 0
  then echo 'Provide domains as args or via stdin, reading from stdin...' >&2
       while read -r domain
       do test -n "$domain" || break
          ${FUNCNAME[0]} "$domain"
       done
       return $?
  fi

  local color
  case "$1" in (--color) color=true; shift;; esac

  for domain; do
    domain=$(echo "$domain" | rev | cut -d. -f-2 | rev)

    date_billed=$(grep "$domain (" cu_invoicelineitems.csv | grep -v SSL | tail -1 | cut -d\" -f8 | sed -sE 's/.* - (.*)\).*/\1/')
    date_created=$(grep ",$domain," "${autodns_csv}" | cut -d, -f3 | cut -dT -f1)
    contact=$(grep ",$domain," "${vautron_csv}" | cut -d, -f3 || grep ",$domain," "${autodns_csv}" | cut -d, -f25)
    renew=$(grep ",$domain," "${vautron_csv}" | cut -d, -f10 || grep ",$domain," "${autodns_csv}" | cut -d, -f6 | cut -dT -f1)
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
    printf "%s${dns:-	}	${dnsa:-	}	${date_billed:-C$date_created}	${renew:-	}	$domain	$contact%s\n" $styling
    #echo "[${style}m${dns:-	}	$(timeout .3s dig +short A "$iddomain" | head -1 | grep . || echo "	")	${date_billed:-C$date_created}	${renew:-	}	$domain	$contact[0m"
  done
}

# Find domains for the user and list basic information
userdomains() {
  user="$1"
  username="$(echo "$user" | cut -d\  -f1)"
  shift
  #CONTACT: local -A contacts=()
  name="$("$HESTIA/bin/v-list-user" "$username" | grep 'FULL NAME:' | cut -d: -f2)"
  if test -t 1 # Avoid special symbols if not a terminal
  then echo "$user" | awk '{print "[4m"$3"	"$1"	'"$(echo $name)"'[0m"}'
  else echo "$user" | awk '{print "\n"$3"	"$1"	'"$(echo $name)"'"}' | tr '[a-z]' '[A-Z]'
  fi

  local color=""
  if test $# -gt 0
  then
    for domain
    do test -z "$domain" || domaininfo "$domain"
    done
    color="--color"
  fi

  local outfile="/tmp/${username}-domains.csv"
  sudo "$HESTIA/bin/v-list-dns-domains" "$username" | tail +3 | sort | while read domain
    do  domain="${domain%% *}"
        if test $(echo "$domain" | tr -cd '.' | wc -c) -ne 1
        then #echo "Ignoring invalid DNS-Domain $domain" >&2
             continue
        fi

        domaininfo $color "$domain"
        #CONTACT: contact=$(grep ",$domain," "${vautron_csv}" | cut -d, -f3 || grep ",$domain," "${autodns_csv}" | cut -d, -f25)
        #CONTACT: if test -n "$contact" && [[ -z "${contacts["$contact"]}" ]]
        #CONTACT: then contacts["$contact"]="$contact"
        #CONTACT: fi
    done | tee "$outfile"
  #CONTACT: for contact in "${!contacts[@]}"; do echo "C $contact"; done

  if test $# -eq 0
  then
    contacts="$outfile-contacts"
    cat "$outfile" | grep -v Bubenheim | rev | cut -d\	 -f1 | rev | sort | uniq | grep . > "$contacts"
    test -s "$contacts" || return
    echo " == Domains for "$(cat "$contacts")" =="
    local count=$(domains --file "$contacts" | tee >(cat >&2) | wc -l)
    local dns_count=$(cat "$outfile" | wc -l)
    test "$count" -eq "$dns_count" ||
      echo "Domains $count / $dns_count"
  fi
}

# Domains of all users, plus list from all providers as of last export
# If arg is provided, only from that domain contact / substring match
domains() {
  if test $# -gt 0
  then grep --no-filename "$@" "${autodns_csv}" "${vautron_csv}" |
         cut -d, -f$(case "$1" in (-*) echo '2-3';; (*) echo '1-3,25';; esac) |
         sort | column -s, -t
       return $?
  fi
  
  set -o pipefail
  sudo "$HESTIA/bin/v-list-users" | tail +3 | grep -v ssh- |
    while read user; do userdomains "$user" ""; done # extra blank param to force color

  echo
  if test -t 1
  then echo "[4mVAUTRON[0m"
  else echo "VAUTRON"
  fi
  for domain in $(cut -d, -f2 "${vautron_csv}")
  do domaininfo "$domain"
  done

  echo
  if test -t 1
  then echo "[4mAUTODNS[0m"
  else echo "AUTODNS"
  fi
  for domain in $(cut -d, -f2 "${autodns_csv}")
  do domaininfo "$domain"
  done
}
