# Infrastructure plan

**Status: plan.** No purchase, provisioning, resize, firewall change, or service
activation is authorized or performed by this document.

Snapshot taken from `doctl` on 2026-07-20:

- `session-server-monolith`: NYC1, 1 vCPU / 1 GiB;
- `ac-lith`: SFO3, 2 vCPU / 4 GiB;
- managed Valkey: SFO3, 1 vCPU / 1 GiB, one node;
- no DigitalOcean load balancer;
- no DigitalOcean database firewall rules currently configured for Valkey;
- no DigitalOcean Cloud Firewall attached to `session-server-monolith`;
- several other AC droplets which should not be treated as spare match hosts;
- production geckos/WebRTC configuration has public STUN but no TURN.

The current fight lobby is process-local memory. Restarting the NYC session
server loses its roster, and a second instance would disagree with it. That is
acceptable for the current prototype, not public matchmaking or tournaments.

## What each service should do

```text
AC identity/API       handle requirement, blocks, moderation, durable results
Lobby/coordinator     presence, queue, rooms, signaling, match lifecycle
Managed Valkey        TTL state, leases, pub/sub, reconnect routing
Cloudflare TURN       global NAT fallback and tournament relay-only privacy
Match verifier        mirrors confirmed inputs, checksums, replay, signed result
Object/database store durable replay/evidence and ratings
```

The coordinator and verifier handle tiny messages and deterministic integer
sims. Player and spectator graphics never run on the server. Spectators receive
confirmed input streams, not video. This keeps compute and bandwidth modest.

## Purchase recommendation

### Development and private alpha: buy no new DigitalOcean compute

Use the existing NYC session server for signaling/coordinator development and
move ephemeral state into the existing managed Valkey. Add Cloudflare Realtime
TURN with short-lived credentials. Its current public pricing includes the first
1,000 GB each month and then charges $0.05/GB; fighting-game input traffic is
small enough that the free tier should cover a substantial alpha.

Do not self-host public TURN on the 1 GiB session droplet. It creates bandwidth,
abuse, credential, firewall, and geographic problems that Cloudflare's anycast
TURN already solves.

### Public crossplay beta: budget about **$33/month additional**

- Add one SFO3 coordinator/verifier droplet at 2 vCPU / 2 GiB: currently about
  **$18/month** on DigitalOcean Basic.
- Put the existing NYC instance and new SFO instance behind a DigitalOcean
  Global Load Balancer: currently **$15/month**.
- Keep the current single-node Valkey for beta, but make every lobby operation
  lease/TTL-based and reconnect-safe.

This gives west/east control-plane presence and rolling deploys. TURN remains
global through Cloudflare. Before buying, load-test WebSocket fan-out and verifier
replay; the recommendation is for operational separation and availability, not
because the sim needs large CPUs.

### Sanctioned tournament launch: budget up to **$90/month additional**

Relative to today's inventory, a conservative North American launch is:

- the new SFO coordinator: +$18;
- resize the NYC coordinator from 1 vCPU / 1 GiB to 2 vCPU / 2 GiB: about +$12;
- Global Load Balancer: +$15;
- upgrade Valkey from today's $15 single node to a 2 GiB primary plus matching
  standby, currently about $60 total: about +$45.

That is approximately **+$90/month** over current spend, plus TURN only if usage
exceeds its free allowance. Add Europe/Asia coordinators only after player
telemetry demonstrates demand; signaling geography matters far less than the
actual direct/TURN match route.

These are planning numbers from current published prices, not a purchase order.
No infrastructure has been created or resized.

## Venue kit

An on-site tournament does not require a cloud region near the venue. Run a
small local coordinator/verifier on the tournament LAN, publish its discovery
record, and sync signed results upstream. Recommended physical requirements:

- managed gigabit Ethernet switch and wired stations;
- local coordinator with battery backup;
- certified displays/controllers/adapters and pinned client builds;
- optional dual-WAN only for identity/bracket continuity;
- encrypted export of match evidence if the internet fails.

## Scaling triggers

Scale from measurement rather than registered-user count:

- coordinator CPU/event-loop delay and WebSocket connection count;
- queue match time by region/rating band;
- TURN selection rate and egress;
- verifier frames simulated per wall-clock second and backlog;
- spectator fan-out bandwidth and catch-up delay;
- Valkey memory, operations, reconnects, and failover behavior;
- match abort/desync/disconnect rates by route and platform.

Do not add Kubernetes, an SFU, or video streaming for v1. Input-stream spectating
and stateless coordinators are much smaller and easier to verify.

## Security issue found during inventory

The current `silo-firewall` inventory includes duplicate inbound MongoDB port
`27017` rules open to `0.0.0.0/0` and `::/0`. Even if Mongo authentication is
enabled, tournament records should not be added there until those rules are
audited and restricted to the actual application sources or private network.

The managed Valkey reports no database firewall rules, and the NYC session
server is not associated with a DigitalOcean Cloud Firewall. They may have
service credentials or host-level firewalling, but those are not substitutes
for verifying the intended network boundary. Audit both before public lobby
state or short-lived TURN credentials are served. This review did not change
any firewall or infrastructure resource.

## Deployment order

1. Build the versioned lobby/room protocol and Valkey state model locally.
2. Add short-lived TURN credentials and measure direct/relay selection.
3. Ship Mac↔Mac private `VS` rooms and public handled-user casual matchmaking.
4. Add server-isolated deterministic spectating.
5. Add verifier/replay and tournament policies.
6. Load-test the current NYC host; buy the SFO coordinator when public beta opens.
7. Add the global load balancer and HA Valkey before sanctioned events.
