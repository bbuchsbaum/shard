# AGENTS.md - Agent Workflow Instructions

This project uses **beads** (`bd`) for git-backed issue tracking. See https://github.com/steveyegge/beads

Run `bd onboard` to get started.

## Essential Commands

| Command | Purpose |
|---------|---------|
| `bd ready` | List tasks without blockers (your next work) |
| `bd create "title" -p 1` | Create task (P0=critical, P1=high, P2=medium, P3=low) |
| `bd show <id>` | View issue details and history |
| `bd update <id> --status in_progress` | Mark task as in progress |
| `bd close <id> --reason "text"` | Close completed task |
| `bd dep add <child> <parent>` | Add dependency |
| `bd list --json` | List all open issues |
| `bd sync` | Force sync to git |

## Critical Rules for Agents

1. **NEVER use `bd edit`** - it opens an interactive editor. Use flag-based updates:
   ```bash
   bd update <id> --description "new description"
   bd update <id> --title "new title"
   ```

2. **Always use `--json` flag** for programmatic access

3. **Run `bd sync` after changes** to ensure immediate git sync

## Finding Work

```bash
bd ready --json          # Tasks without blockers
bd list --status open    # All open tasks
bd stale --days 7        # Neglected tasks
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

