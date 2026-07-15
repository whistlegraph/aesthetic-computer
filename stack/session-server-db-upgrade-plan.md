# Plan: session-server redis 4→6 + mongodb 6→7 upgrade

Status: **blocked / reverted.** A first attempt on 2026-07-15 took production chat
down; rolled back to redis 4.7.1 / mongodb 6.21.0, which is live and stable. This
is the plan to do it properly.

## What happened (the evidence)

On the DO droplet (`157.245.134.225`, `session-server.aesthetic.computer`, single
systemd-managed node process), deploying redis `^6.1.0`:

- The redis v6 client entered a `SocketClosedUnexpectedlyError` reconnect loop on
  its TLS socket (`@redis/client/.../socket.js`).
- The server never bound `:8889` in ~90s → Caddy returned 502 → chat/multiplayer
  down.
- Rollback to redis 4.7.1 restored service.

Two confounds worth stating honestly:

1. **Startup is slow (~45s).** The server loads every chat instance's history
   from Mongo *before* it binds `:8889`. So a slow-but-fine boot can look like a
   failure if you check too early. The redis-6 boot did not recover in the window
   given, but the version's role in *not binding* (vs. merely logging socket
   errors) was not isolated.
2. **`SocketClosedUnexpectedly` appears on redis 4 too.** The managed redis
   closes idle TLS connections; the client reconnects. So that log line alone is
   not the smoking gun — the *tight loop* on v6 is the concern.

## Root cause hypothesis

redis-py-js **v5 changed reconnection and socket semantics** vs v4. The likely
issues:

- No `pingInterval` set → the managed redis (or a proxy) closes idle sockets and
  v6 handles the closure differently than v4.
- `socket.reconnectStrategy` default changed; a tight immediate-retry loop can
  starve the event loop during startup, delaying `fastify.listen()`.
- `rediss://` TLS option handling differs between v4 and v6.

None of this is confirmed — it must be **reproduced off-prod** before retrying.

## The plan

### 1. Reproduce off-prod (do not touch the droplet)
- Stand up a local session-server against a **TLS redis** (managed redis test
  instance, or local redis with TLS + idle-timeout) to reproduce the
  `SocketClosedUnexpectedly` loop under redis 6.
- Confirm whether the loop *prevents* `fastify.listen()` or is merely noisy.

### 2. Fix the client config (session.mjs, ~line 526)
Current: `createClient({ url })` (or argless in dev) for `sub` and `pub`.
Add explicit resilience, e.g.:
```js
createClient({
  url,
  pingInterval: 10_000,               // keep idle TLS sockets alive
  socket: {
    reconnectStrategy: (retries) => Math.min(retries * 200, 5000), // backoff, not tight loop
    keepAlive: 30_000,
  },
})
```
Verify `sub.connect()` / `pub.connect()` don't block startup — consider making
redis connect **non-blocking** (fire-and-forget with the existing `.on("error")`
handlers) so a redis hiccup can never delay `:8889` binding again. That single
change would have prevented the outage regardless of version.

### 3. mongodb 6→7 (lower risk, do separately)
mongodb 7 connected fine in the incident ("MongoDB connected!"). Node engine
requirement is `>=20.19`; droplet runs node v20.17.0 — **check this**, 7.x may
warn or fail on 20.17. Either bump the droplet's node (fnm) to ≥20.19 first, or
hold mongodb at 6.x. Do mongodb separately from redis so a failure is
attributable.

### 4. Decouple boot from history-load (resilience win, independent of upgrade)
Bind `:8889` **first**, then load chat history in the background. Today a slow
Mongo query gates the entire server's availability. This makes every future
deploy safer and faster to health-check.

### 5. Roll out through the new pipeline
Deploy via `session-server/deploy.fish` (health-gated, auto-rollback). It waits
150s for `:8889` and reverts on failure — so even a bad attempt self-heals
instead of leaving chat down. Watch `/tmp/session-server.log` for the redis
connect result and the bind time.

### 6. Also carry the reverted safe changes
The revert bundled these back to old versions for coherence; bring them forward
with the redo (all low-risk): `ip` package removal → `os.networkInterfaces()`,
and the minor bumps (chokidar 5, dotenv 17, fastify 5.10, obscenity 0.4.6, ws
8.21, @geckos.io 3.1). `geoip-lite` must stay 1.x (2.x needs node ≥24).

## Acceptance criteria
- Reproduced and fixed off-prod first.
- Deploy binds `:8889` and returns 200 within the pipeline's health window.
- redis connects without a reconnect loop; pub/sub ("code", "scream") verified.
- Chat + multiplayer reconnect confirmed post-deploy.
