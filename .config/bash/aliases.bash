test -n "$PS1" || return 0

which pfetch >/dev/null 2>&1 && pfetch

alias myip='curl -4 ifconfig.me && printf "\n" && curl -6 ifconfig.me && printf "\n"'

ds() {
	df -B1M -x tmpfs -x devtmpfs -x squashfs -x overlay "$@" |
		{ test "$(df | wc -l)" -gt 100 && grep -v '\b/[^/ ]*/[^/]*/[^/]*$' || cat; } | # needed for NAS to hide overly long submounts
		awk -v a="\033[31m" -v b="\033[33m" -v c="\033[35m" -v n="\033[0m" 'NR==1 {printf "%-20s %6s %7s %9s %s\n",$1,$5,$3,$4,$6} NR>1 {u=$5; printf (u > 98) ? a : (u > 96) ? b : (u > 90) ? c : ""; printf "%-20s %6s %6.1fG %8.1fG %s\n",$1,$5,$3/1024,$4/1024,$6; printf n}' |
		column -t
}
export -f ds &&
	timeout 1s bash -c ds 2>/dev/null

test $(id -u) -eq 0 || sudo=sudo

alias jc="$sudo journalctl --boot --unit"
alias sc="$sudo systemctl"
alias scs="$sudo systemctl status"
alias sce="$sudo systemctl enable --now"
alias sced="$sudo --preserve-env=EDITOR systemctl edit"
alias scr="$sudo systemctl daemon-reload && $sudo systemctl reload-or-restart"

alias hist='history | less'
alias m='mv -vi'

alias nginx-edit="sudo vi /etc/nginx/nginx.conf && nginx -t && sudo systemctl reload nginx"

# Fast Find
ff() {
	name=$1
	shift
	$(command -v fd || echo fdfind) --hidden "$name" ||
	  find "$@" -name "*$name*"
}

xtrace () {
    trap 'set +x' INT
    set -x
    "$@"
    set +x
}

highlight() { echo; echo "[4m$1[0m"; }
status() {
	highlight 'System'
	ds -T
	#df -h -T --exclude-type=tmpfs --exclude-type=devtmpfs --exclude-type=squashfs --exclude-type=overlay
	zfs list -d 0 2>/dev/null
	free -h
	$sudo certbot certificates 2>/dev/null
	test $? -eq 1 && local sudo=""

	highlight 'Internet'
    #--color=always
	ip -brief address | grep --color=none -E '^(wl|en|tun|vmbr)'
	ip route
	echo -n 'IPv4: ' && timeout 3s ping example.com -A -c 3 -w 3 -q -4
	echo -n 'IPv6: ' && timeout 3s ping example.com -A -c 3 -w 3 -q -6

	highlight 'Programs'
	tmux ls 2>/dev/null
	$sudo systemctl --no-pager list-units --failed --no-legend || service --status-all
	echo '== WEBSERVER'
	{ sudo lsof -i :443 || sudo lsof -i :80; } | head -4
	sudo lsof -i :22
	echo
	if type docker &>/dev/null
	then
	  echo '== DOCKER'
	  $sudo docker ps -n 6 || $sudo systemctl status docker
	fi
	if type kubectl &>/dev/null
	then
	  echo '== KUBERNETES NODE'
	  sudo -E kubectl get nodes -o wide
	fi
}

# Find and list disks
alldisks() {
	{
	sudo df -h -T --exclude-type=tmpfs --exclude-type=devtmpfs --exclude-type=squashfs --exclude-type=overlay
	sudo blkid
	sudo fdisk -l
	} | less
}
scandisks() {
	for host in /sys/class/scsi_host/*; do echo "- - -" | sudo tee $host/scan; ls /dev/sd* ; done
}

__u="$sudo apt update && $sudo apt upgrade"
alias u="$__u"
alias ur="tmux new-session -s upgrade '$__u && $sudo reboot'"

alias dif='diff --color=always --side-by-side --report-identical-files'
# Diff recursively
difr() { diff --color=always --unified=1 --recursive "$@" | less --RAW-CONTROL-CHARS --quit-on-intr --quit-if-one-screen; }
# Copy recursively with rsync
alias rc='rsync --recursive --info=progress2,remove,symsafe,flist,del --human-readable --links --hard-links --times'

# duplicates section in .zshenv - TODO unify
export LESS="--raw-control-chars --ignore-case --LONG-PROMPT --jump-target=5 $(test $(less --version | grep -o '[0-9]\+' | head -1) -ge 590 && echo --incsearch)"

# ls aliases

export LS_OPTIONS='--human-readable --si --group-directories-first --dereference-command-line'
command -v dircolors >/dev/null && eval "$(dircolors)"
alias ls='ls --color=auto'
alias ll='ls $LS_OPTIONS --file-type -l'
alias la='ll --all'
alias l='ls $LS_OPTIONS --file-type --color=always --almost-all'

which bat >/dev/null 2>&1 || alias bat="$(which batcat >/dev/null 2>&1 && echo batcat || echo less -FX)"
b() { test -d "${1:-.}" && l "$@" || bat "$@"; }

export EDITOR=${EDITOR:-$(which kak || which nvim || which vi)}
alias v="$(which kak || which nvim || echo ${EDITOR:-vi})"
alias h=man

# Grep aliases

# Default grep with some niceties
alias grpc='grep --color=auto --line-number --binary-files=without-match --directories=skip'
# Default grep with some niceties and ignoring case
alias grp='grpc --ignore-case'
# Grep recursively and paginate
grpr() { grp --color=always --recursive $(echo $DIRS_IGNORE | sed 's/-x/--exclude-dir/g') "$@" | less -FX; }

# Some aliases to avoid making mistakes:

alias rm='rm -I'
alias cp='cp -i'
alias mv='mv -i'

# Completion and Extras

src() { test -f "$1" && source "$1"; }

case $(readlink /proc/$$/exe) in (*bash|"")
bind '"\ek":history-search-backward'
bind '"\ej":history-search-forward'

shopt -oq posix || src /etc/bash_completion

# Fancy prompt
PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]"
PS1="${PS1} \`exitcode=\${?}; if test \$exitcode = 0; then printf '\[\033[01;32m\] :)';"
PS1="${PS1} else printf '\[\033[01;31m\]%3d' \$exitcode; fi\`\[\033[00m\]"
;;
(*zsh) setopt sh_word_split;;
esac

src /usr/share/git/completion/git-prompt.sh && PS1="$PS1\$(__git_ps1 \" (%s)\")"
src $HOME/.config/shell/functions

PS1="${PS1} \`test \$UID = 0 && printf '#' || printf '$'\` "
