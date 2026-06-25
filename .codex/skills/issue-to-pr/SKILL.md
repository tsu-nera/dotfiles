---
name: issue-to-pr
description: "Implement a single GitHub Issue end-to-end in Codex CLI: read and judge issue quality, investigate code, design the change, create an isolated Git worktree, implement there, run checks/tests, commit, push, and open a pull request. Use when the user invokes $issue-to-pr with an issue number or GitHub issue URL, or asks to turn one issue into a PR."
---

# Issue to PR

Use this skill to convert one GitHub Issue into one PR in Codex CLI. Keep the base checkout clean by doing all implementation in an explicit Git worktree.

Do not use Claude Code-only primitives such as `Agent(model: "sonnet", isolation: "worktree")`, `AskUserQuestion`, `EnterPlanMode`, `ExitPlanMode`, or `$CLAUDE_SESSION_ID`. In Codex CLI, isolation means a real `git worktree`. Use Codex sub-agents only when the available tools expose them and the task benefits from delegation.

## Phase 1: Read the Issue

Resolve the issue number from the user input, then run:

```bash
gh issue view <number> --repo <owner/repo>
```

Read comments only if the issue body explicitly says to consult comments:

```bash
gh issue view <number> --repo <owner/repo> --comments
```

Treat the issue as high quality and continue when it has concrete targets, acceptance criteria, and a clear implementation direction. If it is vague, has multiple unresolved approaches, or has unknown blast radius, ask the user a concise plain-text clarification before coding.

## Phase 2: Investigate

Inspect the named files and nearby tests directly for small changes. Use `rg` / `rg --files` first.

For broader changes, spawn `explorer` sub-agents only when the current Codex session exposes sub-agent tools and there are specific independent questions. If sub-agents are unavailable, investigate directly.

Always check for:

- current implementation and local patterns
- tests or sample errors that depend on changed behavior
- repository-specific test guidance such as `docs/test-guidelines.md`
- dirty worktree state in the base checkout

## Phase 3: Design

Decide whether the issue is one PR. If it should be split into independently reviewable PRs, stop and report `SPLIT_NEEDED` with the proposed split.

For one-PR work, write down the concrete implementation plan for yourself:

- files to change
- behavior to preserve
- test commands to run
- expected PR title and branch prefix

If design uncertainty remains, ask the user before creating the worktree. Otherwise continue without confirmation.

## Phase 4: Create the Worktree

Create the implementation worktree from `origin/main`, not from the possibly dirty base checkout:

```bash
git fetch origin main
git worktree add -b <type>/<issue>-<short-topic> <repo-parent>/<repo-name>-<issue> origin/main
```

Examples:

```bash
git worktree add -b refactor/1812-evm-preflight-simulation \
  /home/tsu-nera/repo/xchain-arb-1812 \
  origin/main
```

For Node projects, do not reinstall dependencies in the worktree. Symlink the base checkout's dependencies when present:

```bash
ln -s <base-repo>/node_modules <worktree>/node_modules
```

This symlink will appear as untracked. Never stage it.

## Phase 5: Implement

For simple edits, implement directly in the worktree. For larger edits, either keep implementing directly or, when the current Codex session exposes sub-agent tools, spawn a `worker` sub-agent and explicitly assign:

- worktree path
- files/modules owned by the worker
- exact issue requirements
- design decisions from Phase 3
- commands to run
- commit and PR expectations

Tell the worker that it is not alone in the codebase and must not revert unrelated edits. Make the worker operate only inside the worktree path.

Use `apply_patch` for manual edits. Keep changes scoped to the issue.

## Phase 6: Validate

Run checks inside the worktree.

Prefer project instructions first. If none exist, use manifest defaults:

- Node with `pnpm`: `pnpm run check && pnpm run test`
- otherwise infer from `package.json`, Makefile, or language config

Also run at least one focused regression or smoke command tied to the issue. If `docs/test-guidelines.md` requires inline smoke tests for new I/O code, implement and run them.

Before committing:

```bash
git diff origin/main...HEAD
git status --short
git fetch origin main
git merge-tree --write-tree origin/main HEAD >/dev/null
```

If `merge-tree` fails, merge `origin/main` into the worktree and resolve conflicts. If conflicts are not straightforward, report them.

## Phase 7: Commit and Open PR

Stage only relevant files explicitly:

```bash
git add <changed-file> ...
git commit -m "$(cat <<'EOF'
<type>: <summary>

Closes #<issue>

Co-Authored-By: Codex <noreply@openai.com>
EOF
)"
git push -u origin <branch>
```

Open the PR with a Japanese title/body:

```bash
gh pr create --base main --head <branch> --title "<日本語タイトル>" --body "$(cat <<'EOF'
## 概要
- <変更内容>

Closes #<issue>

## テスト計画
- [x] `<check command>` pass
- [x] `<focused command>` pass
- [x] `<regression command>` pass

---
`🤖 Codex session: unavailable`
EOF
)"
```

If the user or repository expects PR creation approval, stop after validation and present the summary, tests, and proposed PR title/body. Otherwise, open the PR and report the URL.

## Cleanup

Do not remove the worktree before the user reviews the PR unless explicitly asked. When cleanup is requested after merge or abandonment:

```bash
rm <worktree>/node_modules  # only if it is the symlink created for this worktree
git worktree remove <worktree>
git worktree prune
```

Never clean unrelated worktrees.
