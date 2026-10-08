# graphify
- **graphify** (`~/.claude/skills/graphify/SKILL.md`) - any input to knowledge graph. Trigger: `/graphify`
When the user types `/graphify`, use the installed graphify skill or instructions before doing anything else.

# previews
- Never publish Artifacts for previews — serve the local build and open it in the browser instead.

# git
- Never `git reset --hard` — use `--keep`, or a rebase/cherry-pick, so uncommitted work survives.
- Atomic means a commit builds and runs on its own, not that it already matches the target design —
  split wherever the intermediate is green, even when a later commit replaces its behavior.
- Commit the files you changed and only look at those. `git commit --only -- <paths>` isolates
  your hunks without inspecting the rest of the tree; don't read or diff a file you didn't edit.

# agents
- Subagents run on Opus (`model: "opus"`, `{model: 'opus'}` in Workflow `agent()`), Sonnet for mechanical
  or narrow stages; never Fable, not even for hard stages — there are no credits for it.
- Batch verification into one skeptic over a list; never stop a running agent to save credits.
- Before launching each agent, read `~/.claude/state/rate-limits.json` (the statusline's snapshot; `at` is its epoch,
  `used_percentage` / `resets_at` per window): at 5h ≥ 80% or 7d ≥ 90%, hold the launch and wake after `resets_at`.
- Before a large fan-out, present the wave plan and reconfirm it with the user — scope gets re-cut
  mid-session, so check `git worktree list` and recent branches for in-flight work before planning.

# refactoring
- Hand-edit until a change is repetitive at scale; a throwaway codemod for a few dozen sites costs
  more to write, read and trust than the edits it saves.
- A codemod worth writing is committed and carries a `--check` mode. Where a sanctioned tool already
  does the operation and refuses to run, fix the tool — never re-implement it stripped of its checks.
