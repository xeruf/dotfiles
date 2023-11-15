test -n "$PS1" || return 0

which pfetch >/dev/null 2>&1 && pfetch

test $(id -u) -eq 0 || sudo=sudo

alias jc="$sudo journalctl --boot --unit"
alias sc="$sudo systemctl"
alias scs="$sudo systemctl status"
alias sce="$sudo systemctl enable --now"
alias sced="$sudo --preserve-env=EDITOR systemctl edit"
alias scr="$sudo systemctl daemon-reload && $sudo systemctl reload-or-restart"

alias hist='history | less'
alias m='mv -vi'

# Fast Find
ff() {
	name=$1
	shift
	$(command -v fd || echo fdfind) --hidden "$name" ||
	  find "$@" -name "*$name*"
}

highlight() { echo "[4m$1[0m"; }
status() {
	highlight 'System'
	free -h
	df -h -T --exclude-type=tmpfs --exclude-type=devtmpfs --exclude-type=squashfs --exclude-type=overlay
	zfs list -d 0
	sudo certbot certificates
	highlight 'Internet'
    #--color=always
	ip -brief address | grep --color=none -E '^(wl|en|tun|vmbr)'
	ip route
	echo -n 'IPv4: ' && timeout 3s ping example.com -A -c 3 -w 3 -q -4
	echo -n 'IPv6: ' && timeout 3s ping example.com -A -c 3 -w 3 -q -6
	highlight 'Programs'
	tmux ls
	$sudo systemctl --no-pager list-units --failed || service --status-all
	if type docker >/dev/null
	then $sudo docker ps || $sudo systemctl status docker
	fi
}

__u="$sudo apt update && $sudo apt upgrade"
alias u="$__u"
alias ur="tmux new-session -s upgrade '$__u && $sudo reboot'"

# Diff recursively
difr() { diff --color=always --unified=1 --recursive "$@" | less --RAW-CONTROL-CHARS --quit-on-intr --quit-if-one-screen; }
# Copy recursively with rsync
alias rc='rsync --recursive --info=progress2,remove,symsafe,flist,del --human-readable --links --hard-links --times'

export LESS="--RAW-CONTROL-CHARS --ignore-case --LONG-PROMPT --jump-target=5 $(test $(less --version | head -1 | cut -f2 -d' ') -ge 590 && echo --incsearch)"

# ls aliases

export LS_OPTIONS='--human-readable --si --group-directories-first --dereference-command-line'
eval "$(dircolors)"
alias ls='ls --color=auto'
alias ll='ls $LS_OPTIONS --file-type -l'
alias la='ll --all'
alias l='ls $LS_OPTIONS --color=always --almost-all'

which bat >/dev/null 2>&1 || alias bat="$(which batcat >/dev/null 2>&1 && echo batcat || echo less -FX)"
b() { test -d "${1:-.}" && l "$@" || bat "$@"; }

alias v="$(which nvim >/dev/null 2>&1 && echo nvim || echo ${EDITOR:-vi})"
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

case $(readlink /proc/$$/exe) in (*bash)
bind '"\ek":history-search-backward'
bind '"\ej":history-search-forward'

shopt -oq posix || src /etc/bash_completion

# Fancy prompt
PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]"
PS1="$PS1 \`if [ \$? = 0 ]; then echo -e '\[\033[01;32m\]:)';"
PS1="$PS1 else echo -e '\[\033[01;31m\]' \$?; fi\`\[\033[00m\]"
;;
(*zsh) setopt sh_word_split;;
esac

src /usr/share/git/completion/git-prompt.sh && PS1="$PS1\$(__git_ps1 \" (%s)\")"
src $HOME/.config/shell/functions

PS1="$PS1 \$ "
