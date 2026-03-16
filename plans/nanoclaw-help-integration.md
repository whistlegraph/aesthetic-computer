# NanoClaw x help.aesthetic.computer — Integration Plan

**Date:** 2026-03-16
**Status:** Phase 0 complete (local prototype working)

---

## Context

The original `help.aesthetic.computer` was a simple Express server calling Claude Haiku with a static system prompt — no tools, no memory, no codebase access. It runs on a DigitalOcean droplet (146.190.150.173, $24/mo) which is currently **online**.

NanoClaw now has a working HTTP Help channel (`src/channels/http-help.ts`) that replaces the original help server with a full agentic backend:

| Capability | Original Help | NanoClaw Help Channel |
|---|---|---|
| Model | Haiku 4.5 (fixed, API) | Any Claude model via Agent SDK |
| Context | Static system prompt, 20-turn window | Persistent memory + session per user |
| Tools | None (planned but never built) | Full Bash, file read/write, browser |
| Codebase | No access | AC repo mounted read-only in container |
| Auth | Auth0 + handle lookup | Auth0 + sub-based whitelist |
| Isolation | None needed (API-only) | Container per invocation (Docker/Apple Container) |
| Streaming | SSE text chunks | SSE text chunks (same frontend protocol) |

**Key decision:** NanoClaw replaces the original help architecture entirely. The original Phase 2-4 plans (API key storage, RBAC tools, conversation history) are superseded — NanoClaw provides all of this out of the box through its container agent system.

---

## Architecture

```
Browser (help.aesthetic.computer)
  │
  ├── Auth0 login (hi.aesthetic.computer)
  │
  └── POST /api/chat { messages: [...] }
        │
        ▼
  ┌─────────────────────────────────────┐
  │  NanoClaw HTTP Help Channel         │
  │  (Express on port 3004)             │
  │                                     │
  │  1. Validate Auth0 Bearer token     │
  │  2. Check sub against whitelist     │
  │  3. Auto-register user group        │
  │  4. Deliver message to pipeline     │
  │                                     │
  │  ┌───────────────────────────────┐  │
  │  │  NanoClaw Message Loop        │  │
  │  │  → Container Agent            │  │
  │  │    ├─ Claude Agent SDK        │  │
  │  │    ├─ /workspace/group (rw)   │  │
  │  │    ├─ /workspace/extra/       │  │
  │  │    │  aesthetic-computer (ro)  │  │
  │  │    └─ Tools: Bash, Read, etc  │  │
  │  └───────────────────────────────┘  │
  │                                     │
  │  5. Stream response chunks          │
  │  6. End response on completion      │
  └─────────────────────────────────────┘
        │
        ▼
  Caddy reverse proxy (HTTPS + auto-certs)
        │
        ▼
  help.aesthetic.computer
```

### Per-User Isolation

Each authenticated user gets:
- **Own JID:** `http_<sha256(sub)[:16]>@http.help`
- **Own group folder:** `groups/http_help_<hash>/` (persistent memory, CLAUDE.md)
- **Own container session:** Isolated Claude conversation with session resume
- **Own mount:** AC codebase at `/workspace/extra/aesthetic-computer` (read-only)

Users cannot see each other's conversations or memory.

---

## Deployment Options

### Option A: Self-host NanoClaw on VPS (Recommended)

Deploy NanoClaw itself to a VPS. The help channel runs as part of NanoClaw, alongside WhatsApp and any other channels.

**Pros:**
- One service to manage (NanoClaw handles everything)
- WhatsApp + help share the same infrastructure
- Container agents provide real isolation
- Persistent memory across sessions

**Cons:**
- Needs a beefier VPS (Docker + containers = more RAM)
- Requires Docker on the VPS
- More complex than a simple Express server

**Recommended VPS specs:**
- 4 vCPU, 8GB RAM minimum (containers need headroom)
- Ubuntu 24.04
- Docker installed
- ~$48/mo on DigitalOcean (s-4vcpu-8gb)

**Deployment steps:**
1. Provision new VPS (or reuse help droplet after paying DO balance)
2. Install Docker + Node.js 22
3. Clone `whistlegraph/aestheticant` (private fork)
4. Clone `whistlegraph/aesthetic-computer` (for codebase mount)
5. Copy `.env` with all credentials
6. Build container image: `./container/build.sh`
7. Configure Caddy: `help.aesthetic.computer → localhost:3004`
8. Set up systemd service for NanoClaw
9. Verify: `curl https://help.aesthetic.computer/health`

### Option B: Keep help VPS separate, proxy to NanoClaw

Run NanoClaw somewhere else (home server, another VPS, etc.) and have the help VPS proxy API requests to it.

**Pros:**
- Help VPS stays lightweight ($24/mo)
- NanoClaw can run on more powerful hardware

**Cons:**
- Two services to manage
- Latency from VPS → NanoClaw hop
- Need persistent connection or VPN between them

### Option C: Run NanoClaw locally, expose via tunnel

Run NanoClaw on your local machine and expose the help port via Cloudflare Tunnel, Tailscale, or similar.

**Pros:**
- No VPS cost
- Full local hardware (fast containers)
- Already working locally

**Cons:**
- Must keep machine running 24/7
- Depends on home internet uptime
- Not production-grade

---

## Phase Plan

### Phase 0: Local Prototype (DONE)
- [x] HTTP Help channel (`src/channels/http-help.ts`)
- [x] Auth0 token validation with sub-based whitelist
- [x] Auto-registration per user with isolated group folders
- [x] Frontend served from `help/index.html` (identical to original)
- [x] AC codebase mounted read-only into agent containers
- [x] Streaming responses via sendMessage/setTyping lifecycle
- [x] Working locally at `http://localhost:3004`

### Phase 1: Deployment Prep
- [x] DigitalOcean droplets are online
- [ ] Decide deployment option (A/B/C above)
- [ ] Write `deploy.fish` for NanoClaw (based on existing help deploy pattern)
- [ ] Write `provision.fish` for NanoClaw VPS setup (Docker, Node, Caddy)
- [ ] Test full auth flow with production Auth0 (redirect URI must include `help.aesthetic.computer`)
- [ ] Add `help.aesthetic.computer` to Auth0 allowed callback URLs

### Phase 2: Production Deploy
- [ ] Provision VPS + install Docker
- [ ] Deploy NanoClaw + container image
- [ ] Clone AC codebase on VPS for mount
- [ ] Configure Caddy reverse proxy
- [ ] Set up systemd service with auto-restart
- [ ] Verify HTTPS + Auth0 login + chat flow end-to-end
- [ ] Set up log rotation and monitoring

### Phase 3: Polish
- [ ] Add CLAUDE.md to help group folder with AC-specific system prompt
- [ ] Rate limiting (daily message cap per sub via MongoDB or local SQLite)
- [ ] Usage tracking endpoint (`/api/usage`)
- [ ] Handle multiple concurrent users (queue per JID already handles this)
- [ ] Reduce poll latency (currently ~2s from POLL_INTERVAL; add event signal for immediate processing)
- [ ] Error handling UX (container timeout, auth failure, etc.)

### Phase 4: Expand Access
- [ ] Open whitelist to more users (add subs to HELP_ALLOWED_SUBS)
- [ ] Admin panel: manage whitelist via WhatsApp commands to AA
- [ ] Per-user rate limits stored in DB
- [ ] Session-server integration (agentic chat inside aesthetic.computer UI)

---

## Key Files

### NanoClaw (whistlegraph/aestheticant)
| File | Purpose |
|------|---------|
| `src/channels/http-help.ts` | HTTP Help channel implementation |
| `src/channels/registry.ts` | Channel registration system |
| `src/index.ts` | Main orchestrator (message loop, agent invocation) |
| `src/container-runner.ts` | Container spawn + streaming output |
| `help/index.html` | Frontend (copied from AC, works as-is) |
| `.env` | Credentials + help config |

### Aesthetic Computer (whistlegraph/aesthetic-computer)
| File | Purpose |
|------|---------|
| `help/server.mjs` | Original help server (to be replaced) |
| `help/deploy.fish` | Deployment script (template for NanoClaw deploy) |
| `help/provision.fish` | VPS provisioning (template for NanoClaw provision) |
| `plans/help-service.md` | Original plan (superseded by this document) |
| `plans/nanoclaw-help-integration.md` | This plan |

---

## Environment Variables (Production)

```bash
# Claude authentication (pick one)
CLAUDE_CODE_OAUTH_TOKEN=<from claude setup-token>
# or: ANTHROPIC_API_KEY=<api key>

# Assistant identity
ASSISTANT_NAME=AA

# HTTP Help Channel
HELP_PORT=3004
HELP_ALLOWED_SUBS=auth0|63effeeb2a7d55f8098d62f9
HELP_CODEBASE_PATH=/opt/aesthetic-computer
AUTH0_CLIENT_ID=LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt
AUTH0_DOMAIN=aesthetic.us.auth0.com

# WhatsApp (if co-located)
# store/auth/creds.json must exist
```

---

## Chosen Approach: Droplet as Proxy → Macbook

Instead of running NanoClaw on the VPS, the help droplet acts as a thin reverse proxy forwarding to the macbook via SSH tunnel. NanoClaw runs locally with full hardware access.

### How it works

```
Browser → help.aesthetic.computer (HTTPS)
  → Caddy (:443 → :3004)
    → proxy server.mjs (:3004 → :3005)
      → SSH reverse tunnel (:3005 → macbook:3004)
        → NanoClaw HTTP Help channel
```

### Setup steps

1. **On the droplet:** Deploy refactored `help/server.mjs` (thin proxy)
   ```bash
   fish help/deploy.fish
   ```

2. **On the droplet:** Add sandbox SSH key to authorized_keys
   ```bash
   cat help/sandbox-ssh-key.pub | ssh root@help.aesthetic.computer "cat >> ~/.ssh/authorized_keys"
   ```

3. **On the droplet:** Disable ollama and other unused services
   ```bash
   ssh root@help.aesthetic.computer "systemctl stop ollama 2>/dev/null; systemctl disable ollama 2>/dev/null; rm -rf /opt/judge"
   ```

4. **On the macbook:** Start NanoClaw
   ```bash
   cd ~/aestheticant && bash start-nanoclaw.sh
   ```

5. **On the macbook:** Start SSH tunnel
   ```bash
   cd ~/aesthetic-computer && fish help/tunnel.fish
   ```

6. **Verify:** `curl https://help.aesthetic.computer/health`

### Files changed

| File | Change |
|------|--------|
| `help/server.mjs` | Replaced Express+Claude with thin HTTP proxy |
| `help/package.json` | Dropped express, mongodb deps (only dotenv) |
| `help/tunnel.fish` | New: SSH reverse tunnel script for macbook |
| `help/sandbox-ssh-key.pub` | New: sandbox SSH public key |

## Open Questions

1. **Tunnel persistence:** Use `autossh` (brew install autossh) for auto-reconnect, or a launchd plist to keep the tunnel alive.
2. **AC codebase freshness:** The macbook has the live repo — always up to date.
3. **Session-server integration:** The long-term goal is chat inside aesthetic.computer via session-server WebSocket. This would be a second channel (`src/channels/session-server.ts`) connecting via WebSocket to `session-server.aesthetic.computer`. Separate plan needed.
