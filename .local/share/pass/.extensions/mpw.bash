set -e
# Run mpw-cli with information from pass
case "$1" in
	(ftt) mpw="business/ftt/mpw"; shift;;
	(*/*) mpw="$1"; shift;;
	(*) mpw="app/mpw/local";;
esac
passw() {
	printf %s "$(pass show "$mpw" | head -1)" |
		mpw -u "$(pass show "$mpw" | head -2 | tail -1)" -q -m - "$@"
}

test -t 1 && echo "$(passw "$@")" | xclip -sel clip -i

case "$*" in
	(*-t*) echo "$(passw "$@")" ;;
	(*) echo "$(passw -t basic "$@")"
		echo "$(passw -t long "$@")";;
esac
