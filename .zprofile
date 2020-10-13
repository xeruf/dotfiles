# xdg
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
 # adjust programs to use xdg
export XAUTHORITY="$XDG_CACHE_HOME/Xauthority"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java"
export XAUTHORITY="$XDG_CACHE_HOME/Xauthority"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

export TIMEWARRIORDB="$XDG_DATA_HOME/timewarrior"
export TASKRC="$XDG_CONFIG_HOME/task/taskrc"
export TASKDATA="$XDG_DATA_HOME/task"
 # zsh dirs
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export _Z_DATA="$XDG_DATA_HOME/zsh/z"
export HISTFILE="$XDG_DATA_HOME/zsh/history"

# environment
export EDITOR=/usr/bin/nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# red stderr
test -f "/usr/lib/libstderred.so" && export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# software config
 # enable pass extensions
export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
 # fzf defaults
export FZF_DEFAULT_OPTS='--select-1 --exit-0 --tiebreak=end,length --history=/var/tmp/fzf-history --ansi --bind="alt-enter:execute(test -O {} && $EDITOR {} || sudoedit {}),del:execute(gio trash {}),change:top,left-click:execute(xdg-open {})"'
export FZF_DEFAULT_COMMAND="fd --hidden --type file --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
 # ctest
export CTEST_PROGRESS_OUTPUT=1
export CTEST_OUTPUT_ON_FAILURE=1
export CTEST_PARALLEL_LEVEL=3

# Update keyboard layout to US if keyboardio is connected
$HOME/.local/bin/update-keyboard-layout
