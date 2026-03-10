# Claude Code — Global Rules

These rules apply to ALL projects on all machines.

## General

- Prefer CLI-only solutions. Do not suggest GUI steps unless explicitly asked.
- Make minimal changes. Do not refactor, reorganize, or improve adjacent code unless asked.
- No emojis in code, labels, or comments.

## Debugging & Verification

- When fixing a bug, verify the fix works end-to-end before reporting success.
- Run the actual command/workflow and check the output. Do not assume API calls succeed silently.
- Add error checking and log outputs for critical operations.

## Deployment

- Do not deploy or restart services without explicit user confirmation.
- Always ask before deploying changes that could interrupt running operations.
- After deploying, run a health check (curl, docker ps, etc.) to confirm the service is up.
