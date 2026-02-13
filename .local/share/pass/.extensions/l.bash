# Provides a quick overview over the directories in the store
case "$1" in
  ([0-9]) level=$1; shift;;
esac
test $# -gt 0 || arg="-d"
tree "$PASSWORD_STORE_DIR/$1" -C --dirsfirst $arg -L ${level:-1} --noreport "${@:2}" |
  less --quit-if-one-screen
