set -o pipefail

list() {
  type="$1"
  shift
  if test $# -eq 0 && echo $type | grep -q -- '-domains$'
  then for user in $(list users)                               
        do test -t 1 && echo "[4m$user[0m"
           list "$type" "$user"    
        done
  else if test -t 1
          then sudo "/usr/local/hestia/bin/v-list-$type" "$@" | column -t
          else sudo "/usr/local/hestia/bin/v-list-$type" "$@" | tail +3 | awk '{print $1}'
        fi
  fi
}

hestia() {
  test $# -eq 0 && cd /usr/local/hestia && return 0
  set -x
  command=$1
  shift
  sudo $(which $command) "$@"
  set +x
}

accessible() {
  dir=/home/*/web/$1/public_html
  sudo chmod -v 755 $dir
  sudo chown -v :sudo $dir
}
