# Instagram API Migration Progress

**Date:** 2026-03-29
**Status:** Blocked — Instagram rejecting login from all tested IPs

## Summary

Attempted to switch the silo Instagram integration from `@aesthetic.computer` to `@whistlegraph` and upgrade the underlying API library. Login is currently being rejected by Instagram for all attempts.

## What was done

### 1. Credential update
- Updated MongoDB `secrets.instagram` on silo to use `@whistlegraph` account
- Cleared old `@aesthetic.computer` session from `insta-sessions` collection
- Credentials stored in vault at `aesthetic-computer-vault/silo/instagram.env`

### 2. instagram-private-api (existing library)
- Current version: `instagram-private-api@1.46.1` — already the latest on npm
- Library is **unmaintained** (last publish ~2 years ago)
- Login fails with `IgLoginBadPasswordError`
- Initial error: "You can log in with your linked Facebook account" (account was FB-linked)
- After unlinking FB: "We can send you an email to help you get back into your account"
- Error persists with fresh device IDs — library likely doesn't support Instagram's current `enc_password` login flow

### 3. instagram-cli (@i7m/instagram-cli)
- Installed globally on silo: `npm install -g @i7m/instagram-cli`
- Uses Ink (React terminal UI) — requires interactive TTY
- Cannot be driven programmatically or over non-interactive SSH
- Not usable as a library, only as a CLI app

### 4. instagrapi-rest (Python)
- Cloned `subzeroid/instagrapi-rest` to `/opt/instagrapi-rest` on silo
- Pre-built Docker image was ARM-only; built from source instead
- Docker image had Python 3.8 asyncio issues; switched to native venv with system Python 3.12
- Installed `instagrapi 2.3.0` (latest, actively maintained)
- Server runs on `http://localhost:8000` via uvicorn
- Login error: `BadPassword` with message: "change your IP address, because it is added to the blacklist of the Instagram Server"

### 5. Local devcontainer attempt
- Installed `instagrapi 2.3.0` in local devcontainer venv
- External IP: residential (not datacenter)
- Same `BadPassword` error — Instagram may be rejecting the password itself, not just the IP

## Current state on silo

- **silo service**: running normally on port 3003
- **instagrapi-rest**: installed at `/opt/instagrapi-rest` (Python 3.12 venv), can be started with:
  ```bash
  cd /opt/instagrapi-rest && source venv/bin/activate
  uvicorn main:app --host 127.0.0.1 --port 8000
  ```
- **Docker**: installed on silo (was needed for initial attempt, may be useful later)
- **instagram-cli**: installed globally (`instagram-cli auth login -u` for interactive login)
- **Old instagram-private-api**: still in `/opt/silo/node_modules`, still used by `silo/server.mjs`

## Next steps

1. **Verify password** — confirm `@whistlegraph` can log in at instagram.com with the current password
2. **Try interactive login** — SSH into silo directly and run `instagram-cli auth login -u`
3. **Consider instagrapi migration** — if login succeeds via instagrapi, update `silo/server.mjs` to call `localhost:8000` instead of using `instagram-private-api` directly
4. **Systemd service** — create `instagrapi.service` on silo to keep the Python API running
5. **Proxy fallback** — if datacenter IPs remain blocked, set up a residential proxy for Instagram API calls

## Architecture (proposed)

```
silo/server.mjs (port 3003)
  └─ HTTP fetch → instagrapi-rest (port 8000)
       └─ instagrapi 2.3.0 (Python, actively maintained)
            └─ Instagram Private API
```

This replaces the direct `instagram-private-api` npm dependency with a more robust, actively maintained Python backend.
