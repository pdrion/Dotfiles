# /deploy — Deploy to homelab

## Steps

1. Check environment before anything:
   - `df -h /` — abort if less than 5 GB free
   - `docker ps` — check Docker/Colima is running
   - `git status` — check for uncommitted changes, warn if any

2. Show what will be deployed:
   - `git log deploy/main..HEAD --oneline` — list commits to deploy
   - Ask user to confirm

3. Deploy:
   - `git push deploy main`
   - Wait for the post-receive hook to finish

4. Verify deployment:
   - `docker ps` — check the container is running
   - Find the app URL from the Caddyfile and run `curl -s -o /dev/null -w "%{http_code}" <url>`
   - Report success or failure

## Important

- NEVER deploy without user confirmation
- If disk space is below 5 GB, warn and suggest running ~/homelab/scripts/cleanup.sh first
- If git status shows uncommitted changes, ask if the user wants to commit first
