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

---

## Update 2026-04-28 — unblock prep

Re-engaging this migration now that the jeffrey-platter is consuming silo/IG output. Status of the original next-steps list:

1. **Verify password** — *not done* — password hasn't been re-tried since the 2026-03-29 attempts. **User action required**: log into instagram.com as `@whistlegraph` and confirm the password in MongoDB `secrets` (`_id: "instagram"`) actually authenticates without challenge.
2. **Try interactive login** — *not done* — `instagram-cli auth login -u` on silo over SSH is the cleanest path to bootstrap a session that instagrapi can later restore from cookies. **User action required**: SSH to silo and run it after step 1 confirms the password.
3. **instagrapi migration** — *not done* — silo/server.mjs still imports `IgApiClient` from `instagram-private-api`. The HTTP-shim refactor is gated on a successful login (don't refactor speculative code paths).
4. **Systemd service** — *staged* — see [silo/instagrapi.service](../silo/instagrapi.service). Loopback-bound (127.0.0.1:8000); deploy with:
   ```bash
   scp silo/instagrapi.service silo:/etc/systemd/system/instagrapi.service
   ssh silo "systemctl daemon-reload && systemctl enable --now instagrapi"
   systemctl status instagrapi
   ```
5. **Proxy fallback** — *not done* — only relevant if step 1 fails on residential IP too. Skip until needed.

### Runbook for the user

```text
1. Confirm `@whistlegraph` password works at https://www.instagram.com/
   → If yes: continue.
   → If no: reset password, update MongoDB `secrets._id="instagram"`.

2. Deploy instagrapi.service:
     scp silo/instagrapi.service silo:/etc/systemd/system/
     ssh silo "systemctl daemon-reload && systemctl enable --now instagrapi"
     ssh silo "systemctl status instagrapi"
     ssh silo "curl -s http://127.0.0.1:8000/docs | head"   # sanity

3. Bootstrap a session interactively (silo terminal):
     ssh silo
     cd /opt/instagrapi-rest && source venv/bin/activate
     python -c "
     from instagrapi import Client
     cl = Client()
     cl.login('whistlegraph', '<password>')
     cl.dump_settings('/opt/instagrapi-rest/sessions/whistlegraph.json')
     print(cl.user_info_by_username('whistlegraph').full_name)
     "
   → solve any 2FA / email challenge prompts inline.

4. Once a session JSON exists, the silo HTTP-shim refactor can land
   (replace IgApiClient calls in silo/server.mjs with fetches to
   http://127.0.0.1:8000/...). That's a follow-up task; not staged
   here to avoid speculative code that can't be tested without step 3.

5. After silo refactor lands, jeffrey-platter §4 unblocks: bulk-pull
   @whistlegraph and @aesthetic.computer media into
   aesthetic-computer-vault/jeffrey-platter/ig-archive/ for canonical
   image generation.
```

### Why no silo/server.mjs PR yet

The `IgApiClient` → instagrapi-rest swap touches ~200 lines (login, challenge,
2FA, profile, feed, session restore). Doing it before login works means
landing untestable code that may need to change once we see what
instagrapi-rest's actual error shapes look like under failure. Step 3
above gives us a working baseline; the refactor can then be shaped to
match it.
