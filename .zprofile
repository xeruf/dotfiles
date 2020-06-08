. ~/.profile
# red stderr
export LD_PRELOAD="/opt/stderred/build/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# zsh config
export ZDOTDIR="$HOME/.config/zsh"
export _Z_DATA="$HOME/.local/share/zsh/z"
export HISTFILE="$HOME/.local/share/zsh/history"
# programs
export EDITOR=/usr/bin/nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export FZF_DEFAULT_COMMAND="fd --hidden"
export FZF_DEFAULT_OPTS="--select-1 --exit-0 --tiebreak=end,length --history=/var/tmp/fzf-history"
# adjust programs to use xdg
export PASSWORD_STORE_DIR="$HOME/.local/share/pass"
export TIMEWARRIORDB="$HOME/.local/share/timewarrior"

