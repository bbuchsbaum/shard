# AGENTS.md - Agent Workflow Instructions

This project uses **beads** (`bd`) for git-backed issue tracking and
**gastown** (`gt`) for multi-agent orchestration.

- Beads: <https://github.com/steveyegge/beads>
- Gastown: <https://github.com/steveyegge/gastown>

Run `bd onboard` to get started with issue tracking.

------------------------------------------------------------------------

## Gastown Multi-Agent Orchestration

Gastown coordinates multiple Claude Code agents working simultaneously
with persistent context through git-backed storage.

### Key Concepts

| Term        | Definition                                                            |
|-------------|-----------------------------------------------------------------------|
| **Town**    | Management headquarters coordinating workers across Rigs              |
| **Rig**     | Project-specific Git repository (this project is a Rig)               |
| **Mayor**   | Chief coordinator initiating Convoys and distributing work            |
| **Polecat** | Ephemeral worker agent producing Merge Requests in isolated worktrees |
| **Crew**    | Long-lived, named agents maintaining persistent context               |
| **Convoy**  | Work-order grouping related Beads for tracking                        |
| **Hook**    | Special pinned Bead for each agent functioning as work queue          |
| **GUPP**    | “If there is work on your Hook, YOU MUST RUN IT”                      |

### Agent Roles

**Infrastructure Roles** (manage the system): - **Mayor**: Global
coordinator (singleton, persistent) - **Deacon**: Background supervisor
daemon - **Witness**: Per-rig lifecycle manager for workers -
**Refinery**: Per-rig merge queue processor

**Worker Roles** (perform project work): - **Crew**: Persistent workers
with human control; ideal for exploratory work - **Polecat**: Ephemeral
workers managed by Witness; best for discrete tasks

### Essential Gastown Commands

``` bash
# Town & Rig Management
gt install [path]              # Create a new town
gt rig add <name> <url>        # Register project as a rig
gt rig list                    # Show all rigs
gt doctor                      # Run health diagnostics

# Work Assignment
gt sling <bead> <rig>          # Assign work to polecat
gt convoy create "name" [ids]  # Create tracked work batch
gt convoy list                 # Show active convoys
gt convoy status [id]          # Track convoy progress

# Session Management
gt mayor attach                # Start/attach to coordinator
gt handoff                     # Request context cycle
gt peek <agent>                # Check agent health
gt nudge <agent> "message"     # Send message to agent

# Merge Queue
gt mq list [rig]               # Display merge queue
gt mq submit                   # Queue current branch
gt mq status <id>              # Detailed merge request info
```

### The Propulsion Principle (GUPP)

**“If you find work on your hook, YOU RUN IT.”**

Agents execute assigned work immediately without waiting for
confirmation. This makes Gas Town a coordinated, responsive system.

------------------------------------------------------------------------

## Beads Issue Tracking

### Essential Commands

| Command                               | Purpose                                               |
|---------------------------------------|-------------------------------------------------------|
| `bd ready`                            | List tasks without blockers (your next work)          |
| `bd create "title" -p 1`              | Create task (P0=critical, P1=high, P2=medium, P3=low) |
| `bd show <id>`                        | View issue details and history                        |
| `bd update <id> --status in_progress` | Mark task as in progress                              |
| `bd close <id> --reason "text"`       | Close completed task                                  |
| `bd dep add <child> <parent>`         | Add dependency                                        |
| `bd list --json`                      | List all open issues                                  |
| `bd sync`                             | Force sync to git                                     |

### Critical Rules for Agents

1.  **NEVER use `bd edit`** - it opens an interactive editor. Use
    flag-based updates:

    ``` bash
    bd update <id> --description "new description"
    bd update <id> --title "new title"
    ```

2.  **Always use `--json` flag** for programmatic access

3.  **Run `bd sync` after changes** to ensure immediate git sync

### Finding Work

``` bash
bd ready --json          # Tasks without blockers
bd list --status open    # All open tasks
bd stale --days 7        # Neglected tasks
```

------------------------------------------------------------------------

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work
is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1.  **File issues for remaining work** - Create issues for anything that
    needs follow-up

2.  **Run quality gates** (if code changed) - Tests, linters, builds

3.  **Update issue status** - Close finished work, update in-progress
    items

4.  **PUSH TO REMOTE** - This is MANDATORY:

    ``` bash
    git pull --rebase
    bd sync
    git push
    git status  # MUST show "up to date with origin"
    ```

5.  **Clean up** - Clear stashes, prune remote branches

6.  **Verify** - All changes committed AND pushed

7.  **Hand off** - Provide context for next session (use `gt handoff` if
    in gastown)

**CRITICAL RULES:** - Work is NOT complete until `git push` succeeds -
NEVER stop before pushing - that leaves work stranded locally - NEVER
say “ready to push when you are” - YOU must push - If push fails,
resolve and retry until it succeeds
