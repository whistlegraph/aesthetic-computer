# DP1 Feed Architecture in Aesthetic Computer

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    aesthetic.computer Stack                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Main Site                    Session Server        Feed API     │
│  aesthetic.computer           Jamsocket              NEW!        │
│  (Netlify)                    (WebSocket)           (Workers)    │
│       │                            │                     │        │
│       └────────────────┬───────────┴─────────────────────┘        │
│                        │                                          │
│                   [User Request]                                  │
│                        │                                          │
└────────────────────────┼──────────────────────────────────────────┘
                         │
                         ▼
        ┌────────────────────────────────────────┐
        │         feed.aesthetic.computer         │
        │     (Cloudflare Worker - Edge API)      │
        ├────────────────────────────────────────┤
        │                                         │
        │  • Hono Framework (TypeScript)         │
        │  • Bearer Token Auth                   │
        │  • Ed25519 Signing                     │
        │  • OpenAPI 3.1.0                       │
        │  • DP-1 v1.0.0 Compliant               │
        │                                         │
        └──────┬──────────────────────┬──────────┘
               │                      │
               │                      │
    ┌──────────▼─────────┐   ┌───────▼──────────┐
    │   KV Storage       │   │   Queue System   │
    │  (3 Namespaces)    │   │   (Async Ops)    │
    ├────────────────────┤   ├──────────────────┤
    │                    │   │                  │
    │ • Playlists        │   │ • Write Queue    │
    │ • Channels         │   │ • Background     │
    │ • Playlist Items   │   │   Processing     │
    │                    │   │                  │
    └────────────────────┘   └──────────────────┘
```

## Request Flow

```
1. Client Request
   │
   └──> feed.aesthetic.computer/api/v1/playlists
        │
        ├──> [GET] Read Request
        │    │
        │    └──> KV Storage
        │         │
        │         └──> Return JSON Response
        │
        └──> [POST] Write Request
             │
             ├──> Validate Auth (Bearer Token)
             │    │
             │    ├──> Success
             │    │    │
             │    │    ├──> Sync Mode (default)
             │    │    │    │
             │    │    │    ├──> Write to KV
             │    │    │    └──> Return 201 Created
             │    │    │
             │    │    └──> Async Mode (Prefer: respond-async)
             │    │         │
             │    │         ├──> Queue Write Operation
             │    │         └──> Return 202 Accepted
             │    │
             │    └──> Failure
             │         └──> Return 401 Unauthorized
             │
             └──> Sign with Ed25519
```

## Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│                        aesthetic.computer                     │
│                         (Main Site)                          │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │ Fetch playlists
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                  feed.aesthetic.computer                     │
│                    (Worker API)                              │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  GET /api/v1/playlists          POST /api/v1/playlists      │
│       │                              │                       │
│       │                              │ + Bearer Token        │
│       ▼                              ▼                       │
│  ┌─────────────────────┐    ┌─────────────────────┐        │
│  │   KV: Playlists     │◄───│   Ed25519 Signing   │        │
│  │                     │    │   + Validation       │        │
│  │  {                  │    └─────────────────────┘        │
│  │    "id": "abc...",  │                                     │
│  │    "title": "...",  │                                     │
│  │    "items": [...],  │                                     │
│  │    "signature": ... │                                     │
│  │  }                  │                                     │
│  └─────────────────────┘                                     │
│                                                               │
└───────────────────────────────────────────────────────────────┘
```

## Deployment Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    Your Dev Container                         │
│                (Fedora Linux with Dev Tools)                  │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  /workspaces/aesthetic-computer/                             │
│  │                                                            │
│  ├── system/           (Netlify Site)                        │
│  │   └── netlify/functions/                                  │
│  │                                                            │
│  ├── session-server/   (Jamsocket WebSocket)                │
│  │                                                            │
│  ├── nanos/            (GCP Unikernels)                      │
│  │   └── conductor.mjs (Cloudflare DNS Manager)              │
│  │                                                            │
│  └── dp1-feed/         (NEW! Cloudflare Worker)              │
│      ├── worker.ts     (Entry point)                         │
│      ├── wrangler.toml (Config)                              │
│      ├── setup-resources.fish (Setup script)                 │
│      └── deploy-feed.fish (Deploy script)                    │
│                                                               │
└────────────┬─────────────────────────────────────────────────┘
             │
             │ npm run feed:deploy
             │
             ▼
┌──────────────────────────────────────────────────────────────┐
│                    Cloudflare Network                         │
│                   (Global Edge Network)                       │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Worker: aesthetic-feed                                      │
│  Domain: feed.aesthetic.computer                             │
│  │                                                            │
│  ├── KV Namespaces (3)                                       │
│  │   ├── DP1_PLAYLISTS                                       │
│  │   ├── DP1_CHANNELS                                        │
│  │   └── DP1_PLAYLIST_ITEMS                                  │
│  │                                                            │
│  └── Queue                                                    │
│      └── dp1-write-operations-prod                           │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

## Security Flow

```
                    Client Request
                         │
                         ▼
            ┌────────────────────────┐
            │  feed.aesthetic.com    │
            └────────┬───────────────┘
                     │
        ┌────────────┴────────────┐
        │                         │
        ▼                         ▼
   [Read API]              [Write API]
   No Auth                 Auth Required
        │                         │
        │                    ┌────┴─────┐
        │                    │          │
        │                    ▼          ▼
        │              API Token    JWT Token
        │              (Bearer)     (Bearer)
        │                    │          │
        │                    └────┬─────┘
        │                         │
        │                    Validate
        │                         │
        │                    ┌────┴────┐
        │                    │         │
        ▼                    ▼         ▼
    KV Read              Success    Fail
    Return 200          Sign Data   401
                        Write KV
                        Return 201
```

## Integration Points

```
┌─────────────────────────────────────────────────────────────┐
│                    aesthetic.computer                        │
│                      (Main Platform)                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Piece Code (JavaScript)                                     │
│  │                                                           │
│  └──> fetch('https://feed.aesthetic.computer/api/v1/...')  │
│        │                                                     │
│        ├──> Read playlists                                  │
│        │    const playlists = await response.json()         │
│        │                                                     │
│        ├──> Display content                                 │
│        │    playlists.forEach(p => render(p))               │
│        │                                                     │
│        └──> Create new playlists (with API key)             │
│             fetch(..., {                                    │
│               method: 'POST',                                │
│               headers: { Authorization: 'Bearer ...' }      │
│             })                                               │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

## Development Workflow

```
Local Dev                    →  Testing        →  Production
│                               │                  │
├─ npm run feed:dev            ├─ npm test        ├─ npm run feed:deploy
│  (localhost:8787)            │                  │
│                              │                  │
├─ Edit TypeScript             ├─ API tests       ├─ Wrangler deploy
│  worker.ts, app.ts           │  api.test.ts     │  to Cloudflare
│                              │                  │
├─ Hot reload                  ├─ Integration     ├─ Custom domain
│  Instant updates             │  tests           │  feed.aesthetic.computer
│                              │                  │
└─ Local KV/Queue              └─ Benchmarks      └─ Global edge network
   In-memory                      k6 load tests      Real KV/Queue
```

## File Structure

```
dp1-feed/
├── worker.ts                   # Cloudflare Worker entry point
├── server.ts                   # Node.js server entry (optional)
├── app.ts                      # Main application logic (Hono)
├── wrangler.toml               # Cloudflare config (UPDATED)
├── package.json                # Dependencies
│
├── routes/                     # API route handlers
│   ├── playlists.ts
│   ├── channels.ts
│   └── items.ts
│
├── storage/                    # KV abstraction layer
│   └── kv-storage.ts
│
├── queue/                      # Queue handlers
│   └── write-queue.ts
│
├── middleware/                 # Auth & validation
│   ├── auth.ts
│   └── cors.ts
│
├── scripts/                    # Utility scripts
│   ├── test-api.js
│   └── k6-benchmark.js
│
├── setup-resources.fish        # NEW! Setup wizard
├── deploy-feed.fish            # NEW! Deploy script
└── AESTHETIC-COMPUTER-SETUP.md # NEW! Setup guide
```

---

**Legend:**
- `│ └──>` Data flow
- `[Component]` Processing step
- `(Description)` Component detail
- `NEW!` Newly added for aesthetic.computer
