test -n "$PS1" || return 0

export LESS="--RAW-CONTROL-CHARS --ignore-case --LONG-PROMPT --jump-target=5 $(test $(less --version | head -1 | cut -f2 -d' ') -ge 590 && echo --incsearch)"
bind '"\ek":history-search-backward'
bind '"\ej":history-search-forward'

if test -f /etc/bash_completion && ! shopt -oq posix
then . /etc/bash_completion
fi

alias sc="sudo systemctl"
alias scs="sudo systemctl status"
alias sce="sudo systemctl enable --now"
alias scr="sudo systemctl reload-or-restart"

# Diff recursively
difr() { diff --color=always --unified=1 --recursive "$@" | less --RAW-CONTROL-CHARS --quit-on-intr --quit-if-one-screen; }
# Copy recursively with rsync
alias rc='rsync --recursive --info=progress2,remove,symsafe,flist,del --human-readable --links --hard-links --times'

# ls aliases

export LS_OPTIONS='--human-readable --si --group-directories-first --file-type --dereference-command-line'
eval "$(dircolors)"
alias ls='ls --color=auto'
alias ll='ls $LS_OPTIONS -l --all'
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

# Fancy prompt

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]"
PS1="$PS1 \`if [ \$? = 0 ]; then echo -e '\[\033[01;32m\]:)';"
PS1="$PS1 else echo -e '\[\033[01;31m\]' \$?; fi\`\[\033[00m\]"

src() { test -f "$1" && source "$1"; }
src /usr/share/git/completion/git-prompt.sh && PS1="$PS1\$(__git_ps1 \" (%s)\")"
src $HOME/.config/shell/functions

PS1="$PS1 \$ "
