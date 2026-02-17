# CLAUDE.md - Project Context for Claude

## Multi-Agent Orchestration (Gastown)

This project is managed with **gastown** for multi-agent coordination.
See <https://github.com/steveyegge/gastown>

### Quick Reference

``` bash
gt mayor attach              # Start/attach to coordinator
gt sling <bead> <rig>        # Assign work to agent
gt convoy create "name"      # Create work batch
gt convoy list               # Track progress
gt handoff                   # Context cycle/session refresh
gt mq submit                 # Queue branch for merge
```

### Key Concepts

- **Mayor**: Coordinator that breaks down work and manages agents
- **Polecat**: Ephemeral worker for discrete tasks
- **Crew**: Persistent worker for exploratory/long-running work
- **Convoy**: Batch of related work items for tracking
- **Hook**: Your work queue - if work is on it, run it immediately
  (GUPP)

### The Propulsion Principle

**“If there is work on your Hook, YOU MUST RUN IT.”** Execute assigned
work immediately without waiting for confirmation.

------------------------------------------------------------------------

## Issue Tracking (Beads)

This project uses **beads** for git-backed issue tracking. See AGENTS.md
for full workflow.

``` bash
bd list                   # List open issues
bd ready                  # Find next task (no blockers)
bd create "title" -p 1    # Create issue (P0-P3 priority)
bd show <id>              # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>             # Close issue
bd sync                   # Sync to git
```

**Critical**: Never use `bd edit` (interactive). Use
`bd update <id> --field "value"` instead.

------------------------------------------------------------------------

## Session Management

When ending a work session, always “land the plane”:

1.  File issues for remaining work

2.  Run quality gates

3.  Update issue status

4.  Sync and push:

    ``` bash
    bd sync
    git pull --rebase
    git push
    ```

5.  Provide handoff context (or use `gt handoff`)

**Work is NOT complete until `git push` succeeds.**
