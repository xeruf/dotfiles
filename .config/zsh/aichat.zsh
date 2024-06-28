# https://github.com/sigoden/aichat/blob/main/scripts/shell-integration/integration.zsh
_aichat_zsh() {
    if [[ -n "$BUFFER" ]]; then
        local _old=$BUFFER
        BUFFER+="⌛"
        zle -I && zle redisplay
        BUFFER=$(aichat -e "$_old")
        zle end-of-line
    fi
}
zle -N _aichat_zsh
# bindkey '\ee' _aichat_zsh
# Original above, this inspired by jsgpt
bindkey '^o' _aichat_zsh
