# ls aliases

export LS_OPTIONS='--human-readable --si --group-directories-first --file-type --dereference-command-line'
eval "$(dircolors)"
alias ls='ls --color=auto'
alias ll='ls $LS_OPTIONS -l --all'
alias l='ls $LS_OPTIONS --color=always --almost-all'
which bat >/dev/null || alias bat=batcat
b() {
	test -d "${1:-.}" && l "$@" || bat "$@"
}
alias v=nvim

# Some aliases to avoid making mistakes:

alias rm='rm -I'
alias cp='cp -i'
alias mv='mv -i'

# Fancy prompt

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]"
PS1="$PS1 \`if [ \$? = 0 ]; then echo -e '\[\033[01;32m\]:)';"
PS1="$PS1 else echo -e '\[\033[01;31m\]' \$?; fi\`\[\033[00m\]"

src() { test -f "$1" && source "$1"; }
src /usr/share/git/completion/git-prompt.sh && PS1="$PS1\$(__git_ps1 \" (%s)\")"
src $HOME/.config/shell/functions

PS1="$PS1 \$ "
