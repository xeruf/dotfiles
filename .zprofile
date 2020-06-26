. ~/.profile
# xdg
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
# zsh config
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export _Z_DATA="$XDG_DATA_HOME/zsh/z"
export HISTFILE="$XDG_DATA_HOME/zsh/history"
# environment
export EDITOR=/usr/bin/nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# software config
export FZF_DEFAULT_OPTS='--select-1 --exit-0 --tiebreak=end,length --history=/var/tmp/fzf-history --ansi --bind="alt-enter:execute(test -O {} && $EDITOR {} || sudoedit {}),del:execute(gio trash {}),change:top"'
export FZF_DEFAULT_COMMAND="fd --hidden --type file --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# adjust programs to use xdg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export TIMEWARRIORDB="$XDG_DATA_HOME/timewarrior"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
# red stderr
export LD_PRELOAD="/opt/stderred/build/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"

$HOME/.local/bin/update-keyboard-layout.sh
