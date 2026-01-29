# CLAUDE.md - Project Context for Claude

## Issue Tracking

This project uses **beads** for issue tracking. See AGENTS.md for full workflow.

```bash
bd list                   # List open issues
bd ready                  # Find next task (no blockers)
bd create "title" -p 1    # Create issue (P0-P3 priority)
bd show bd-xxx            # View issue details
bd close bd-xxx           # Close issue
bd sync                   # Sync to git
```

## Session Management

When ending a work session, always "land the plane":

1. File issues for remaining work
2. Run quality gates
3. Update issue status
4. `bd sync && git push`
5. Provide handoff context for next session

**Work is NOT complete until `git push` succeeds.**
