# Commands

neofetch
task next limit:3
timew

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
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

ZSH_THEME="powerlevel10k/powerlevel10k"
#POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator context dir rbenv vcs) POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs time)
#ZSH_CUSTOM=$ZSH/custom

HIST_STAMPS="yyyy-mm-dd" # history command timestamps

# COMPLETION
# ENABLE_CORRECTION="true" # Correct command arguments
HYPHEN_INSENSITIVE="true" # - and _ interchangeable
COMPLETION_WAITING_DOTS="true" # Dots while waiting for completion
DISABLE_UNTRACKED_FILES_DIRTY="true" # DOn't mark untracked files as dirty - speeds up status check


# Plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
plugins=(
	git
	git-extras
	z
	fast-syntax-highlighting
	zsh-autosuggestions
	#zsh-vim-mode
)

SHELL_CONFIG="$HOME/.config/shell"
ZSH_CONFIG="$HOME/.config/zsh"

_comp_options+=(globdots) # Show files starting with dot in autocomplete
fpath=($fpath "$ZSH_CONFIG/zsh_completion") # Custom completions
ZSH_COMPDUMP="$(xdg-user-dir CACHE)/zsh/zcompdump-$ZSH_VERSION" # Cache completions

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

export KEYTIMEOUT=1

# Manual VIM bindings - disabled, replaced by plugin {{{
#bindkey -v
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
# }}}

# Show time on the right after executing command - obsolete through powerlevel10k {{{
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
# }}}

## User configuration

source "$SHELL_CONFIG/functions"

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


# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi


# AUTOMATICALLY ADDED SHIT

if [ -d "~/.sdkman" ]; then
    export SDKMAN_DIR="~/.sdkman"
    [[ -s "~/.sdkman/bin/sdkman-init.sh" ]] && source "~/.sdkman/bin/sdkman-init.sh"
fi

# added by travis gem
[ -f /home/janek/.travis/travis.sh ] && source /home/janek/.travis/travis.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# To customize prompt, run `p10k configure` or edit .p10k.zsh.
[[ ! -f $ZSH_CONFIG/.p10k.zsh ]] || source $ZSH_CONFIG/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
