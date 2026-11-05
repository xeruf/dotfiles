# git
- Never `git reset --hard` — use `--keep`, or a rebase/cherry-pick, so uncommitted work survives.
- Atomic means a commit builds and runs on its own, not that it already matches the target design —
  split wherever the intermediate is green, even when a later commit replaces its behavior.
- Commit the files you changed and only look at those. 
  `git commit --only -- <paths>` isolates your hunks without inspecting the rest of the tree; 
  don't read or diff a file you didn't edit unless your work relates to it.

# agents
- Run subagents on Opus (`model: "opus"`, `{model: 'opus'}` in Workflow `agent()`), 
  Sonnet for scoped web research, mechanical or narrow stages
- Batch verification into one skeptic over a list
- Do not stop or interrupt a running agent midway without confirmation
- Before launching agents, read `~/.claude/state/rate-limits.json` 
  (the statusline's snapshot; `at` is its epoch, `used_percentage` / `resets_at` per window):
  at 5h ≥ 80% or 7d ≥ 90%, hold the launch and wake after `resets_at`.
  When 5h ≥ 50%, launch agents sequentially one at a time rather than in waves.

# files
- Never store results meant for me in /tmp or the scratchpad; put them in a local folder,
  e.g. a sibling of the project directory.
- Put intermediates in the project's local build folder (e.g. `build/`, `target/`, `dist/`) if one exists;
  fall back to the scratchpad only otherwise.

# tools
- `sudo` and `rm -r` are deliberately blocked. Never attempt them, not even a probe;
  hand me the complete command to run myself.
- **graphify** (`~/.claude/skills/graphify/SKILL.md`) - any input to knowledge graph. Trigger: `/graphify`
When the user types `/graphify`, use the installed graphify skill or instructions before doing anything else.
