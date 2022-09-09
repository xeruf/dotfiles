export DATA="$(test -d $HOME/daten && echo $HOME/daten || echo $HOME/data)"
export MUSIC="$DATA/4-media/music"

export BORG_REPO='/mnt/backup/borg'
export BORG_PASSCOMMAND='pass service/device/borg/backup'

# xdg
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.local/cache"
export XDG_CONFIG_HOME="$HOME/.config"

export JOURNAL="$(eval "dirname $(grep -1 journals $XDG_CONFIG_HOME/jrnl/jrnl.yaml | tail -1 | cut -d':' -f2-)" ||
	echo "$DATA/2-box/journal")"
export INSTALEE_HOME="$DATA/1-projects/1-personal/instalee"
test -d "/mnt/data/projects/vosk/models" &&
	export VOSK_MODELS="/mnt/data/projects/vosk/models"
 # adjust programs to use xdg
export MNT=/run/media/$USER
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

export LYNX_CFG_PATH="$XDG_CONFIG_HOME"/lynx.cfg
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export RLWRAP_HOME="$XDG_DATA_HOME"/rlwrap
export LESSHISTFILE="$XDG_STATE_HOME"/lesshst

 ## Graphical
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export KDEHOME="$XDG_STATE_HOME"/kdehome
export DOOMLOCALDIR="$XDG_STATE_HOME"/emacs
export WINEPREFIX="$XDG_DATA_HOME"/wine

 ## Development tools
export GOPATH="$XDG_STATE_HOME"/go
export KREW_ROOT="$XDG_DATA_HOME"/krew

export CARGO_HOME="$XDG_STATE_HOME"/cargo
export RUSTUP_HOME="$XDG_STATE_HOME"/rustup

export NVM_DIR="$XDG_DATA_HOME"/nvm
export N_PREFIX="$XDG_STATE_HOME"/n

export CABAL_CONFIG="$XDG_CONFIG_HOME"/cabal/config
export CABAL_DIR="$XDG_CACHE_HOME"/cabal

export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_STATE_HOME"/bundle

export PYTHONSTARTUP="$XDG_CONFIG_HOME"/pythonstartup.py
export KSCRIPT_CACHE_DIR="$XDG_CACHE_HOME"/kscript

export RBENV_ROOT="$XDG_STATE_HOME"/rbenv

 # Java & Android
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java"
export GRADLE_USER_HOME="$XDG_STATE_HOME"/gradle
export ANDROID_PREFS_ROOT="$XDG_CONFIG_HOME"/android
export ANDROID_EMULATOR_HOME="$XDG_STATE_HOME"/android/emulator
export ANDROID_SDK_ROOT="/opt/android/sdk"
 # taskwarrior & timewarrior
export TIMEWARRIORDB="$XDG_DATA_HOME"/timewarrior
export TASKRC="$XDG_CONFIG_HOME"/task/taskrc
export TASKDATA="$XDG_DATA_HOME"/task
 # zsh dirs
export ZDOTDIR="$XDG_CONFIG_HOME"/zsh
export ZSH="$XDG_DATA_HOME"/zsh/oh-my-zsh
export HISTFILE="$XDG_STATE_HOME"/zsh/history
export CONFIG_ZSH="$XDG_CONFIG_HOME"/zsh
export CONFIG_SHELLS="$XDG_CONFIG_HOME"/shell
mkdir -p "$XDG_STATE_HOME/zsh"

# environment
BIN="$HOME/.local/bin"
export PATH="$BIN/scripts:$BIN:$RBENV_ROOT/shims:$PATH:$XDG_CONFIG_HOME/emacs/bin:$N_PREFIX:$GOPATH/bin:$XDG_DATA_HOME/gem/ruby/3.0.0/bin:$ANDROID_SDK_ROOT/platform-tools:$CARGO_HOME/bin:$KREW_ROOT/bin"
export ALTERNATE_EDITOR="$(
	if which nvim >/dev/null
	then echo nvim
	else echo vi
	fi)"
export EDITOR="$(
	if which emacs >/dev/null
	then echo $BIN/scripts/emacstty
	else echo $ALTERNATE_EDITOR
	fi
)"
export LESS="--RAW-CONTROL-CHARS --ignore-case --LONG-PROMPT --jump-target=5 $(test $(less --version | head -1 | cut -f2 -d' ') -ge 590 && echo --incsearch)"
 # TODO put into config file and use --exclude-from
export DIRS_GENERATED="-x generated -x .gradle -x cmake_build -x dist-newstyle -x node_modules -x __pycache__"
export DIRS_IGNORE_SAFE="-x .cache -x .cpan -x *Cache -x .pyenv -x .local/cache -x share/baloo -x share/cabal -x share/cargo -x share/digikam -x share/gem -x share/JetBrains -x share/tldr -x share/syncthing -x share/Steam/ubuntu* -x share/Steam/package -x share/virtualenv -x share/Zeal -x state/gradle -x state/android -x Ferdi/Partitions -x oh-my-zsh -x wine/drive_c/windows -x vendor/cache $DIRS_GENERATED"
export DIRS_IGNORE="-x .sync -x .stfolder -x *build -x .git -x .idea -x env -x out -x cache -x Partitions -x vendor/bundle -x log $DIRS_IGNORE_SAFE"
# red stderr
test -f "/usr/lib/libstderred.so" && export LD_PRELOAD="/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# software config
export OSFONTDIR="$XDG_DATA_HOME"/fonts:/usr/share/fonts
export TEXMF=/usr/share/texmf-dist
export KSCRIPT_IDEA_COMMAND=intellij-idea-ultimate-edition
## enable pass extensions
export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
## man
export MANPAGER="less --squeeze-blank-lines +Gg"
export LESS_TERMCAP_mb=$'\e[1;31m'	# begin bold
export LESS_TERMCAP_md=$'\e[1;32m'	# begin blink
export LESS_TERMCAP_so=$'\e[36m'	# bottom indication and search highlight
export LESS_TERMCAP_us=$'\e[4;33m'	# begin underline
export LESS_TERMCAP_me=$'\e[0m'	# reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'	# reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'	# reset underline
export GROFF_NO_SGR=1				# for konsole and gnome-terminal
## fzf defaults
[[ "$(fzf --version 2>/dev/null | grep --only-matching '[0-9]\.[^. ]*')" > 0.31 ]] && _fzf_latest=true || _fzf_latest=false
FZF_BINDINGS=$(echo "
change:top
alt-enter:execute(test -O {} && $EDITOR {} || sudoedit {})
alt-bspace:execute(gio trash {})
double-click:execute(xdg-open {})
ctrl-a:select-all
ctrl-l:kill-line
alt-a:select-all
alt-c:yank
alt-w:toggle-preview-wrap
ctrl-alt-h:backward-kill-word
$($_fzf_latest && echo "shift-down:preview-half-page-down,shift-up:preview-half-page-up
alt-j:preview-half-page-down,alt-k:preview-half-page-up
alt-shift-down:preview-down,alt-shift-up:preview-up,esc:close")
" | xargs -I% echo -n "%," | head -c-1)
#alt-r:preview(bat {}),
export FZF_HISTDIR="$XDG_STATE_HOME/fzf"
mkdir -p "$XDG_STATE_HOME/fzf"
export FZF_DEFAULT_OPTS="--select-1 --exit-0 --ansi
--tiebreak=$($_fzf_latest && echo "chunk,")end,length --history=$FZF_HISTDIR/history --bind='$FZF_BINDINGS'
$($_fzf_latest && echo '--preview-window=60%,border-left --marker=o')"
FD_BASE="fd --hidden --color=always --no-ignore-vcs"
export FZF_DEFAULT_COMMAND="$FD_BASE --type file"
export FZF_CTRL_T_COMMAND="$FD_BASE -d 7"
## ctest
export CTEST_PROGRESS_OUTPUT=1
export CTEST_OUTPUT_ON_FAILURE=1
export CTEST_PARALLEL_LEVEL=3
