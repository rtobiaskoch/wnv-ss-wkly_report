# Plan: Regression Guard Hooks

## Context

Two recurring failure modes from session history:
1. **Destructive bash commands ran without warning** — `git filter-repo` permanently deleted `.hessian` files with no confirmation step. A `PreToolUse` hook on Bash intercepts commands matching known-dangerous patterns and blocks them, forcing explicit user confirmation.
2. **Refactor regressions caught too late** — edits to `utils/*.R` (dedup logic, join functions, expansion steps) silently broke test-passing invariants (trap counts inflated, malfunction rows dropped) and were only noticed on manual review. A `PostToolUse` hook on Edit/Write auto-runs `Rscript tests/run_tests.R` immediately after any `utils/*.R` edit, surfacing failures before the next edit lands.

---

## Hook 1: Destructive Bash Guard (global)

**Scope:** `~/.claude/settings.json` — applies to all projects.

### New script: `~/.claude/hooks/protect_destructive_bash.sh`

```bash
#!/bin/bash
# Blocks destructive bash commands and forces user confirmation.
# Patterns: git filter-repo, rm -rf, git reset --hard, force-push variants.

input=$(cat)
command=$(echo "$input" | jq -r '.tool_input.command // ""')

if echo "$command" | grep -qE '(git filter-repo|rm -rf|git reset --hard|git push --force|git push -f[[:space:]]|git push -f$)'; then
  jq -n --arg cmd "$command" \
    '{"continue": false, "stopReason": ("BLOCKED: destructive command requires explicit user confirmation before running.\n\nCommand: " + $cmd + "\n\nTell the user what you are about to do and why, list every file/path that could be affected, and wait for approval.")}'
  exit 2
fi

exit 0
```

### Addition to `~/.claude/settings.json` hooks section

Merge into existing `PreToolUse` array (currently only has the `protect_lockfiles` hook on `Edit|Write`). Add a new entry:

```json
{
  "matcher": "Bash",
  "hooks": [{
    "type": "command",
    "command": "bash ~/.claude/hooks/protect_destructive_bash.sh",
    "timeout": 5,
    "statusMessage": "Checking for destructive commands..."
  }]
}
```

---

## Hook 2: Auto-run Tests After `utils/*.R` Edit (project)

**Scope:** `.claude/settings.json` (new project-level file, committed to git) — applies only to this repo.

### New file: `.claude/settings.json`

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "file_path=$(jq -r '.tool_input.file_path // .tool_response.filePath // \"\"'); if echo \"$file_path\" | grep -qE 'utils/[^/]+\\.R$'; then cd /Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report && Rscript tests/run_tests.R 2>&1; fi",
            "timeout": 120,
            "statusMessage": "Running utils/ unit tests..."
          }
        ]
      }
    ]
  }
}
```

**Why synchronous (not async):** The whole point is to block Claude from continuing if tests fail. Async would let Claude make additional edits before seeing the failure — the same problem we have now.

**Why 120s timeout:** R startup + testthat loading takes a few seconds; the full test suite may take 30–60s depending on fixture size.

---

## Verification Steps

### After Hook 1 (destructive bash guard):
1. Pipe-test the raw script: `echo '{"tool_name":"Bash","tool_input":{"command":"rm -rf /tmp/test"}}' | bash ~/.claude/hooks/protect_destructive_bash.sh` — should exit 2 and print blocking JSON.
2. Safe command should pass: `echo '{"tool_name":"Bash","tool_input":{"command":"git status"}}' | bash ~/.claude/hooks/protect_destructive_bash.sh` — should exit 0 silently.
3. Validate settings JSON: `jq -e '.hooks.PreToolUse[] | select(.matcher == "Bash")' ~/.claude/settings.json`

### After Hook 2 (test runner):
1. Pipe-test: `echo '{"tool_name":"Edit","tool_input":{"file_path":"/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_calc_abund.R"}}' | bash -c '<inline command>'` — should run tests and print testthat output.
2. Non-utils file should be a no-op: same test with `file_path` set to a `.qmd` file — should exit 0 with no output.
3. Validate project settings JSON: `jq -e '.hooks.PostToolUse[0].matcher' .claude/settings.json`
4. Live proof: Edit a single character in `utils/fun_calc_abund.R`, observe tests fire automatically, revert the edit.

---

## Files Changed

| File | Action |
|------|--------|
| `~/.claude/hooks/protect_destructive_bash.sh` | Create new script |
| `~/.claude/settings.json` | Merge new PreToolUse Bash entry into existing hooks |
| `.claude/settings.json` | Create new project-level settings file |

Note: `.claude/settings.json` (project) does not yet exist. After creation, verify it is not gitignored (it should be committed so the test-runner guard travels with the repo).
