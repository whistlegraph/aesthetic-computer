# Fleet dashboard — design note

A read-mostly web view of the fleet, sibling to the auth/status dashboards on
`lith.aesthetic.computer`. It answers, at a glance and for humans, the same
question `fleet-mcp` answers for agents: *what machines exist, what can they do,
which are alive right now.*

## What it shows

- **Machine tiles**, grouped by `designation` (agent-endpoint · compute-node ·
  control · build · service · display · legacy). Each tile:
  - emoji + label + canonical name
  - live status dot: 🟢 online · ⚪ offline · ❔ unknown, with **last-seen**
    relative time for offline nodes (from `tailscale status`)
  - `designation` badge + capability chips (`gpu`, `mlx`, `chromium-pool`, …)
  - hardware line (chip / cores / memory) and OS
  - a `⚠ review` flag for entries `normalize-machines.mjs` hasn't confirmed
- **Filter bar**: by designation, by capability chip, online-only toggle. Same
  vocabulary as `fleet_find`.
- **Fleet header counts**: N online / M total, and per-designation tallies.
- Optional **agent/identity column** once hermes lands: which agent-endpoint
  owns which identity, last heartbeat.

Deliberately **not** shown in the public/tailnet page: raw IPs, ssh key paths,
passwords, droplet IDs. Those stay server-side (vault) — the page renders
labels, designations, capabilities, and liveness only. (PII-out rule.)

## Where it lives — recommendation

**Recommended: a tailnet-gated page, not a public lith route.**

Two viable homes:

1. **lith Express route** — add `system/netlify/functions/fleet.mjs` (lith
   adapts function files as routes; the path is historical). It would read the
   vault registry server-side + shell `tailscale status --json` on whatever host
   serves it. *Problem:* lith is public-internet facing and does not run on the
   tailnet, so (a) it can't see `tailscale status` for @jeffrey's tailnet, and
   (b) a fleet map is exactly the private inventory the "slab is public / keep
   PII out" rule wants off the public web. Workable only as a JSON API behind
   real auth, fed by a pushed status blob — more plumbing than it's worth for v1.

2. **A small page served on the tailnet from `jasellite`** (the always-on
   services appliance already exposing authed tailnet APIs on ports like
   `:7765`). It has first-hand `tailscale status`, already holds the vault-style
   private config, and is only reachable by devices on the tailnet — so tailnet
   membership *is* the auth boundary. **This is the recommendation for v1.**

   Shape: a `/api/fleet` JSON endpoint that runs the exact `fleet-mcp.mjs` merge
   logic (factor the registry+tailscale merge into a shared `fleet.mjs` module
   that both the MCP and the endpoint import), plus a static single-file HTML
   dashboard that polls it every ~10s. Zero new datastore — the vault file +
   live `tailscale status` are the whole backend.

## Auth considerations

- **v1:** tailnet membership is the gate (page bound to the tailnet interface on
  jasellite; not exposed publicly). No extra login needed — if you can reach it,
  you're on the tailnet.
- **If it must go on lith/public** later: put it behind the same session/handle
  auth the other lith dashboards use, and serve a **redacted** projection only
  (labels + designation + capabilities + online dot — never IPs/keys). Feed it a
  status blob that jasellite pushes up on a timer, rather than giving the public
  box tailnet visibility.
- Ties into `AC shared session` (`~/.ac-token`) if per-viewer identity is ever
  wanted, but that's overkill for a personal tailnet inventory.

## Build order (when picked up)

1. Extract the merge (`loadRegistry` + `tailscaleStatus` + `liveFor`) from
   `fleet-mcp.mjs` into a shared `fleet.mjs`; have the MCP import it.
2. Add `GET /api/fleet` on jasellite returning the redacted JSON projection.
3. Static HTML dashboard (tiles + filter + poll). No framework needed.
