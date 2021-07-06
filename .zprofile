# xdg
export DATA="$HOME/data"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export PATH="$HOME/.local/bin/scripts:$HOME/.local/bin:$PATH:$XDG_CONFIG_HOME/emacs/bin"
 # adjust programs to use xdg
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

export KDEHOME="$XDG_DATA_HOME"/kdehome
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export GOPATH="$XDG_DATA_HOME"/go

export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export RLWRAP_HOME="$XDG_CACHE_HOME"/rlwrap
export CABAL_CONFIG="$XDG_CONFIG_HOME"/cabal/config
export CABAL_DIR="$XDG_CACHE_HOME"/cabal

export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle

 # Java & Android
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java"
export GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle
export ANDROID_PREFS_ROOT="$XDG_CONFIG_HOME"/android
export ANDROID_EMULATOR_HOME="$XDG_DATA_HOME"/android/emulator
export ANDROID_SDK_ROOT="/opt/android-sdk"
export PATH="$PATH:$ANDROID_SDK_ROOT/platform-tools"
 # taskwarrior & timewarrior
export TIMEWARRIORDB="$XDG_DATA_HOME/timewarrior"
export TASKRC="$XDG_CONFIG_HOME/task/taskrc"
export TASKDATA="$XDG_DATA_HOME/task"
 # zsh dirs
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export HISTFILE="$XDG_DATA_HOME/zsh/history"
export ZSH="$XDG_DATA_HOME/zsh/oh-my-zsh"
export CONFIG_ZSH="$XDG_CONFIG_HOME/zsh"
export CONFIG_SHELLS="$XDG_CONFIG_HOME/shell"
 # ccache - can be removed with version 4 - https://github.com/ccache/ccache/issues/191
export CCACHE_CONFIGPATH="$XDG_CONFIG_HOME"/ccache.config
export CCACHE_DIR="$XDG_CACHE_HOME"/ccache

# environment
which nvim >/dev/null && export EDITOR='nvim' || export EDITOR='vim'
export LESS='--RAW-CONTROL-CHARS --ignore-case --incsearch --LONG-PROMPT --jump-target=5'
export IGNOREDIRS=".sync,.stfolder,.git,out,build,dev"
# red stderr
test -f "/usr/lib/libstderred.so" && export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# software config
## enable pass extensions
export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
## man
export MANPAGER="less --squeeze-blank-lines +Gg"
export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
export LESS_TERMCAP_so=$'\e[36m'       # bottom blue
export LESS_TERMCAP_us=$'\e[01;37m'    # begin underline
export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
## fzf defaults
FZF_BINDINGS=$(echo '
change:top
alt-enter:execute(test -O {} && $EDITOR {} || sudoedit {})
alt-bspace:execute(gio trash {})
double-click:execute(xdg-open {})
ctrl-a:select-all
alt-a:select-all
alt-c:yank
alt-w:toggle-preview-wrap
alt-j:preview-half-page-down,alt-k:preview-half-page-up
shift-down:preview-half-page-down,shift-up:preview-half-page-up
alt-shift-down:preview-down,alt-shift-up:preview-up
esc:close
' | xargs -I% echo -n "%," | head -c-1)
#alt-r:preview(bat {}),
export FZF_DEFAULT_OPTS="--select-1 --ansi --marker=o
--tiebreak=end,length --history=/var/tmp/fzf-history --bind='$FZF_BINDINGS'
--preview-window=60%,border-left"
FD_BASE="fd --hidden --color=always --no-ignore-vcs"
export FZF_DEFAULT_COMMAND="$FD_BASE --type file"
export FZF_CTRL_T_COMMAND="$FD_BASE -d 7"
## ctest
export CTEST_PROGRESS_OUTPUT=1
export CTEST_OUTPUT_ON_FAILURE=1
export CTEST_PARALLEL_LEVEL=3

if test -z "${DISPLAY}" && test "${XDG_VTNR}" -eq 1 ; then
  echo "What do you want to do?"
  while test $(echo "$intention" | wc -c) -lt 6
  do read intention
  done
  jrnl intentions "$intention"
  exec startx
fi
