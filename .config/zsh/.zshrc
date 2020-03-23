# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.local/share/oh-my-zsh
DEFAULT_USER=$USER

ZSH_COMPDUMP="/var/tmp/zcompdump-$ZSH_VERSION"
ZSH_DISABLE_COMPFIX=true

ZSH_THEME="powerlevel10k/powerlevel10k"
export TERM="xterm-256color"

#POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator context dir rbenv vcs)
#POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs time)

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"
# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  git-extras
  z
  fast-syntax-highlighting
  zsh-autosuggestions
  zsh-vim-mode
)

SHELL_CONFIG="$HOME/.config/shell"

fpath=($fpath "$SHELL_CONFIG/zsh_completion")

source $ZSH/oh-my-zsh.sh

## Functions

tab_list_files() {
  if [[ $#BUFFER == 0 ]]; then
    BUFFER="ls "
    CURSOR=3
    zle list-choices
    zle backward-kill-word
  elif [[ $BUFFER =~ ^[[:space:]][[:space:]].*$ ]]; then
    BUFFER="./"
    CURSOR=2
    zle list-choices
    [ -z ${TAB_LIST_FILES_PREFIX+x} ] && BUFFER="  " CURSOR=2
  elif [[ $BUFFER =~ ^[[:space:]]*$ ]]; then
    BUFFER="cd "
    CURSOR=3
    zle list-choices
    [ -z ${TAB_LIST_FILES_PREFIX+x} ] && BUFFER=" " CURSOR=1
  else
    BUFFER_=$BUFFER
    CURSOR_=$CURSOR
    zle expand-or-complete || zle expand-or-complete || {
      BUFFER="ls "
      CURSOR=3
      zle list-choices
      BUFFER=$BUFFER_
      CURSOR=$CURSOR_
    }
  fi
}
zle -N tab_list_files
bindkey '^I' tab_list_files

fancy-ctrl-z() {
  if [[ $#BUFFER -eq 0 ]]; then
    bg
    zle redisplay
  else
    zle push-input
  fi
}
zle -N fancy-ctrl-z
bindkey '^z' fancy-ctrl-z
bindkey '^q' push-line-or-edit

#bindkey -v
export KEYTIMEOUT=1

#autoload -Uz history-search-end
#
#zle -N history-beginning-search-backward-end history-search-end
#zle -N history-beginning-search-forward-end history-search-end
#
#bindkey -M vicmd '^[[A' history-beginning-search-backward-end \
#                 '^[OA' history-beginning-search-backward-end \
#                 '^[[B' history-beginning-search-forward-end \
#                 '^[OB' history-beginning-search-forward-end
#bindkey -M viins '^[[A' history-beginning-search-backward-end \
#                 '^[OA' history-beginning-search-backward-end \
#                 '^[[B' history-beginning-search-forward-end \
#                 '^[OB' history-beginning-search-forward-end

# Show time on the right after executing command
# strlen() {
#   FOO=$1
#   local zero='%([BSUbfksu]|([FB]|){*})'
#   LEN=${#${(S%%)FOO//$~zero/}}
#   echo $LEN
# }
# preexec() {
#   DATE=$( date +"[%H:%M:%S]" )
#   local len_right=$( strlen "$DATE" )
#   len_right=$(( $len_right+1 ))
#   local right_start=$(($COLUMNS - $len_right))
#
#   local len_cmd=$( strlen "$@" )
#   local len_prompt=$(strlen "$PROMPT" )
#   local len_left=$(($len_cmd+$len_prompt))
#
#   RDATE="\033[${right_start}C ${DATE}"
#
#   if [ $len_left -lt $right_start ]; then
#     # command does not overwrite right prompt - ok to move up one line
#     echo -e "\033[1A${RDATE}"
#   else
#     echo -e "${RDATE}"
#   fi
# }


## User configuration

source "$SHELL_CONFIG/functions"

#fpath=($fpath "$SHELL_CONFIG/zsh_completion")
#compinit -d /var/tmp/zcompdump-$ZSH_VERSION

# turn on spelling correction
setopt correct
# don't save duplicates in command history
setopt histignoredups

setopt extended_glob
# Enable zmv
autoload zmv
alias zmv='noglob zmv'
alias zmw='zmv -W'
alias zcp='noglob zmv -C'
alias zln='noglob zmv -L'
alias zsy='noglob zmv -Ls'

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

if [ -d "~/.sdkman" ]; then
    export SDKMAN_DIR="~/.sdkman"
    [[ -s "~/.sdkman/bin/sdkman-init.sh" ]] && source "~/.sdkman/bin/sdkman-init.sh"
fi

# added by travis gem
[ -f /home/janek/.travis/travis.sh ] && source /home/janek/.travis/travis.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f $SHELL_CONFIG/p10k.zsh ]] || source $SHELL_CONFIG/p10k.zsh

PATH=$PATH:/home/janek/daten/programme/010editor;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 38562BC7-22FC-AE71-D3CD-79BBBCE2AB2A
