source ~/.local/share/shell/functions

# ls aliases

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Fancy prompt

PS1="\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]"
PS1="$PS1 \`if [ \$? = 0 ]; then echo -e '\[\033[01;32m\]:)';"
PS1="$PS1 else echo -e '\[\033[01;31m\]:(' \$?; fi\`\[\033[00m\]"
PS1="$PS1 \$(__git_ps1 \"(%s)\") \$ "
