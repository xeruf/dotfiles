#!/bin/sh
input=$(cat)

user=$(whoami)
host=$(hostname -s)
dir=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
dir=$(basename "$dir")
model=$(echo "$input" | jq -r '.model.display_name // ""')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Git branch (skip optional lock)
git_branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')" symbolic-ref --short HEAD 2>/dev/null)

# user@host:dir  (matches PS1 green user@host + blue working dir)
printf "\033[01;34m%s\033[00m" "$dir"

# git branch
if [ -n "$git_branch" ]; then
  printf " \033[01;32m(%s)" "$git_branch"
fi

# model name
if [ -n "$model" ]; then
  printf " \033[00;36m%s\033[00m" "$model"
fi

# context usage
if [ -n "$used" ]; then
  printf " \033[00;35mctx:%s%%\033[00m" "$used"
fi

printf "\n"
