. ~/.profile
# xdg
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
 # adjust programs to use xdg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export TIMEWARRIORDB="$XDG_DATA_HOME/timewarrior"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
 # zsh dirs
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export _Z_DATA="$XDG_DATA_HOME/zsh/z"
export HISTFILE="$XDG_DATA_HOME/zsh/history"

# environment
export EDITOR=/usr/bin/nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# red stderr
export LD_PRELOAD="/opt/stderred/build/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# software config
 # fzf defaults
export FZF_DEFAULT_OPTS='--select-1 --exit-0 --tiebreak=end,length --history=/var/tmp/fzf-history --ansi --bind="alt-enter:execute(test -O {} && $EDITOR {} || sudoedit {}),del:execute(gio trash {}),change:top,left-click:execute(xdg-open {})"'
export FZF_DEFAULT_COMMAND="fd --hidden --type file --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
 # ctest
export CTEST_PROGRESS_OUTPUT=1
export CTEST_OUTPUT_ON_FAILURE=1
export CTEST_PARALLEL_LEVEL=3

# Update keyboard layout to US if keyboardio is connected
$HOME/.local/bin/update-keyboard-layout.sh
