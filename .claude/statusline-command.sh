#!/bin/sh
input=$(cat)

eval "$(echo "$input" | jq -r '@sh "
cwd=\(.workspace.current_dir // .cwd // ".")
model=\(.model.display_name // "")
effort=\(.effort.level // "")
fast=\(.fast_mode // false)
name=\(.session_name // "")
dur_ms=\(.cost.total_duration_ms // 0)
api_ms=\(.cost.total_api_duration_ms // 0)
ctx=\(.context_window.used_percentage // "" | tostring)
ctx_size=\(.context_window.context_window_size // 200000)
style=\(.output_style.name // "")
agent=\(.agent.name // "")
wt=\(.worktree.name // .workspace.git_worktree // "")
added=\(.cost.total_lines_added // 0)
removed=\(.cost.total_lines_removed // 0)
h5=\(.rate_limits.five_hour.used_percentage // "" | tostring)
h5_reset=\(.rate_limits.five_hour.resets_at // "" | tostring)
d7=\(.rate_limits.seven_day.used_percentage // "" | tostring)
d7_reset=\(.rate_limits.seven_day.resets_at // "" | tostring)
cache_seen=\(.prompt_cache.caching_observed // false)
cache_warm=\(.prompt_cache.warm // false)
cache_exp=\(.prompt_cache.expires_at // "" | tostring)
cache_misses=\(.prompt_cache.misses // 0)
"')"

now=$(date +%s)

# why: the rate-limit gauges reach only this script; a snapshot lets an orchestrating session read them before launching agents
if [ -n "$h5$d7" ]; then
  snap=~/.claude/state/rate-limits.json
  echo "$input" | jq -c --argjson at "$now" '{at: $at, five_hour: .rate_limits.five_hour, seven_day: .rate_limits.seven_day}' > "$snap.$$" && mv "$snap.$$" "$snap"
fi
branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$cwd" symbolic-ref --short HEAD 2>/dev/null)
ahead=$(GIT_OPTIONAL_LOCKS=0 git -C "$cwd" rev-list --count '@{u}..HEAD' 2>/dev/null)
dirty=$(GIT_OPTIONAL_LOCKS=0 git -C "$cwd" status --porcelain 2>/dev/null | wc -l | tr -d ' ')
worktrees=$(GIT_OPTIONAL_LOCKS=0 git -C "$cwd" worktree list --porcelain 2>/dev/null | grep -c '^worktree ')

# bar <percent> [label]: 5-cell bar, green <60, yellow <85, red above; label defaults to the percent
bar() {
  p=${1%.*}
  if [ "$p" -ge 85 ]; then c=31; elif [ "$p" -ge 60 ]; then c=33; else c=32; fi
  filled=$(( (p + 10) / 20 ))
  s=""; i=0
  while [ $i -lt 5 ]; do
    if [ $i -lt $filled ]; then s="${s}▰"; else s="${s}▱"; fi
    i=$((i + 1))
  done
  printf "\033[%sm%s %s\033[00m" "$c" "$s" "${2:-$p%}"
}

# tokens <count>: 76k / 1M
tokens() {
  if [ "$1" -ge 1000000 ]; then printf "%dM" $(($1 / 1000000)); else printf "%dk" $(($1 / 1000)); fi
}

# span <seconds>: 1h05m / 42m
span() {
  if [ "$1" -ge 3600 ]; then printf "%dh%02dm" $(($1 / 3600)) $(($1 % 3600 / 60))
  else printf "%dm" $(($1 / 60)); fi
}

# human <seconds>: rounded duration — minutes under 100m, then hours, then days above 40h
human() {
  s=$1
  if [ "$s" -lt 6000 ]; then
    printf "%dm" $(( (s + 30) / 60 ))
  elif [ "$s" -lt 144000 ]; then
    printf "%dh" $(( (s + 1800) / 3600 ))
  else
    printf "%dd" $(( (s + 43200) / 86400 ))
  fi
}

# line 1: where and who
printf "\033[01;34m%s\033[00m" "$(basename "$cwd")"
if [ -n "$branch" ]; then
  printf " \033[01;32m(%s" "$branch"
  [ -n "$ahead" ] && [ "$ahead" -gt 0 ] && printf " ↑%s" "$ahead"
  [ -n "$dirty" ] && [ "$dirty" -gt 0 ] && printf " ●%s" "$dirty"
  printf ")\033[00m"
  [ "$worktrees" -gt 1 ] && printf " \033[00;33m⎇%s\033[00m" $((worktrees - 1))
fi
[ -n "$wt" ] && printf " \033[00;33mwt:%s\033[00m" "$wt"
if [ -n "$model" ]; then
  printf "  \033[00;36m%s" "$model"
  [ -n "$effort" ] && printf " %s" "$effort"
  [ "$fast" = true ] && printf " ⚡"
  printf "\033[00m"
fi
case "$style" in ""|[Dd]efault) ;; *) printf "  \033[00;33mstyle:%s\033[00m" "$style" ;; esac
[ -n "$agent" ] && printf "  \033[00;33magent:%s\033[00m" "$agent"
[ -n "$name" ] && printf "  \033[00;35m%s\033[00m" "$name"
[ "$dur_ms" -ge 60000 ] && printf "  \033[02m%s (%s api)\033[00m" "$(human $((dur_ms / 1000)))" "$(human $((api_ms / 1000)))"
[ "$added$removed" != "00" ] && printf "  \033[32m+%s\033[00m \033[31m−%s\033[00m" "$added" "$removed"
printf "\n"

# line 2: gauges, skipped until the first response fills them
gauges() {
  [ -n "$ctx" ] && printf "ctx " && bar "$ctx" "$(tokens $((ctx_size * ${ctx%.*} / 100)))/$(tokens "$ctx_size")"

  if [ -n "$h5" ]; then
    printf "  5h " && bar "$h5"
    if [ -n "$h5_reset" ]; then
      remaining=$(( h5_reset - now )); [ $remaining -lt 0 ] && remaining=0
      printf "\033[02m ↻%s\033[00m" "$(human $remaining)"
    fi
  fi

  if [ -n "$d7" ]; then
    printf "  7d " && bar "$d7"
    if [ -n "$d7_reset" ]; then
      remaining=$(( d7_reset - now )); [ $remaining -lt 0 ] && remaining=0
      printf "\033[02m ↻%s\033[00m" "$(human $remaining)"
      # why: ▲ when usage runs ahead of the share of the week already elapsed
      elapsed=$(( (604800 - (d7_reset - now)) * 100 / 604800 ))
      [ "${d7%.*}" -gt $((elapsed + 5)) ] && printf " \033[01;31m▲\033[00m"
    fi
  fi

  if [ "$cache_seen" = true ]; then
    if [ "$cache_warm" = true ] && [ -n "$cache_exp" ] && [ "$cache_exp" -gt "$now" ]; then
      printf "  \033[02m♨ %s\033[00m" "$(span $((cache_exp - now)))"
    else
      printf "  \033[00;34m❄ cold\033[00m"
    fi
    [ "$cache_misses" -gt 0 ] && printf " \033[31m✗%s\033[00m" "$cache_misses"
  fi
}
line2=$(gauges)
[ -n "$line2" ] && printf "%s\n" "$line2"
