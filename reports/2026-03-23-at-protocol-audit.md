# AT Protocol Integration Audit

**Date:** 2026-03-23
**Scope:** at.aesthetic.computer PDS, ATProto sync, tooling, lexicons, CLI

---

## 1. Current State Overview

Aesthetic Computer runs a **self-hosted PDS** (Personal Data Server) at `at.aesthetic.computer` on a DigitalOcean droplet. The integration is production-grade with:

- 6 custom lexicons under `computer.aesthetic.*`
- Bidirectional sync between MongoDB and PDS for paintings, moods, tapes, kidlisp, pieces, news
- User account provisioning (handle format: `{handle}.at.aesthetic.computer`)
- Guest/anonymous content via `art.at.aesthetic.computer`
- 31+ admin scripts, 28+ sync/verification scripts
- User profile pages at `{handle}.at.aesthetic.computer`

**PDS Stack:** DigitalOcean droplet (1 CPU, 1GB RAM, 25GB SSD — $6/mo) + Spaces blob storage ($5/mo)

---

## 2. Architecture

### Data Flow (Hybrid Model)

```
User Action → AC Backend (Netlify Functions)
                ├── MongoDB  (application data, search, features)
                ├── PDS      (identity, ATProto records, federation)
                └── DO Spaces (media blobs — PNG, ZIP, MP4)
```

### Key Backend Modules

| Module | Location | Purpose |
|--------|----------|---------|
| `at.mjs` | `system/backend/at.mjs` | Account CRUD, handle sync |
| `media-atproto.mjs` | `system/backend/media-atproto.mjs` | Unified record creation for all 5 media types |
| `painting-atproto.mjs` | `system/backend/painting-atproto.mjs` | Painting-specific sync with thumbnails |
| `mood-atproto.mjs` | `system/backend/mood-atproto.mjs` | Mood sync |
| `tape-atproto.mjs` | `system/backend/tape-atproto.mjs` | Tape sync with MP4 conversion |
| `news-atproto.mjs` | `system/backend/news-atproto.mjs` | News record sync |

### Netlify Functions Touching ATProto

| Function | What It Does |
|----------|-------------|
| `handle.mjs` | Syncs AC handle changes → PDS handle |
| `mood.mjs` | Creates mood + syncs to PDS |
| `store-kidlisp.mjs` | Stores KidLisp + syncs to PDS |
| `store-piece.mjs` | Stores piece + syncs to PDS |
| `tv.mjs` / `tv-tapes.mjs` | Queries tapes from PDS via XRPC |
| `atproto-user-stats.mjs` | Aggregate user stats from MongoDB |
| `delete-erase-and-forget-me.mjs` | Full account deletion including PDS |

---

## 3. Custom Lexicons

All live at `at/lexicons/computer/aesthetic/`:

| Lexicon | Required Fields | Blob? |
|---------|----------------|-------|
| `computer.aesthetic.painting` | slug, code, imageUrl, when, ref | thumbnail (PNG/JPEG, 1MB max) |
| `computer.aesthetic.mood` | mood, when, ref | — |
| `computer.aesthetic.piece` | slug, when, ref | — |
| `computer.aesthetic.kidlisp` | code, source, acUrl, when, ref | — |
| `computer.aesthetic.tape` | slug, code, acUrl, when, ref | video (MP4), thumbnail |
| `computer.aesthetic.news` | headline, when, ref | — |

### Lexicon Publication Status

- **DNS TXT record** (`_lexicon.aesthetic.computer`) is planned but status unclear — needs verification.
- Lexicon JSON schemas exist locally but may not be published to the network yet.

---

## 4. Accounts & Identity

| Account | Handle | DID | Purpose |
|---------|--------|-----|---------|
| Main | `aesthetic.computer` | `did:plc:k3k3wknzkcnekbnyde4dbatz` | Official Bluesky presence |
| Art (guest) | `art.at.aesthetic.computer` | `did:plc:tliuubv7lyv2uiknsjbf4ppw` | Anonymous/guest content |
| Users | `{handle}.at.aesthetic.computer` | `did:plc:*` | Per-user PDS accounts |

### Account Provisioning Flow

1. User registers on AC (Auth0)
2. `createAtprotoAccount()` generates invite code, creates PDS account
3. Credentials stored in MongoDB: `users.atproto = { did, handle, password, created }`
4. Handle changes sync via `updateAtprotoHandle()`
5. Account deletion cascades to PDS via `deleteAtprotoAccount()`

---

## 5. Tooling Audit

### Existing Scripts (Scattered, No Unified CLI)

**`at/` directory** — standalone query/post tools:
- `query-profile.mjs` — fetch profile for any handle/DID
- `query-posts.mjs` — fetch posts from account
- `post-to-bluesky.mjs` — post to @aesthetic.computer
- `resolve-did.mjs` — resolve DID documents, inspect PDS
- `explore-lexicons.mjs` — browse lexicon schemas
- `share-latest-painting.mjs` / `share-random-painting.mjs`
- `test-mood-api.mjs` / `test-painting-atproto.mjs` / `test-all.mjs`

**`at/pds/scripts/`** — admin tools (31 scripts):
- Account: `create-account.mjs`, `create-bulk-accounts.mjs`, `recreate-jeffrey.mjs`
- Invites: `generate-invite.mjs`
- Checks: `check-missing-accounts.mjs`, `check-stale-atproto.mjs`, `check-handle-structure.mjs`
- Migration: `migrate-pds-accounts.mjs`, `migrate-atproto-to-nested.mjs`
- Monitoring: `health-check.sh`, `backup.sh`
- Queries: `query-user.mjs`, `explore-collections.mjs`, `inspect-user-schema.mjs`

**`scripts/atproto/`** — sync/backfill scripts (28 scripts):
- Backfill: `backfill-moods-to-atproto.mjs`, `backfill-paintings-to-atproto.mjs`
- Verify: `verify-paintings-sync.mjs`, `verify-tapes-sync.mjs`
- Compare: `compare-moods-atproto-mongo.mjs`
- Stats: `mood-sync-stats.mjs`, `painting-sync-stats.mjs`
- Sync: `sync-atproto.mjs`, `sync-mongodb-from-atproto.mjs`

### Problem: No Unified Entry Point

All ~60+ scripts require knowing exact paths and running `node path/to/script.mjs`. There is no `ac-at` CLI analogous to `ac-os`, `papers/cli.mjs`, or `memory/cli.mjs`. This makes the tooling:

- **Hard to discover** — you need to know what exists and where
- **Inconsistent invocation** — some use dotenv, some need env vars, some need SSH
- **No help system** — no way to list available commands
- **Duplicated patterns** — similar arg parsing, PDS URL resolution, etc. in each script

---

## 6. Issues & Improvement Opportunities

### P0 — Verify & Fix

- [ ] **PDS health**: Run health check against `at.aesthetic.computer` — confirm it's online and responding
- [ ] **DNS TXT record**: Verify `_lexicon.aesthetic.computer` TXT record exists for lexicon publication
- [ ] **Wildcard DNS**: Confirm `*.at.aesthetic.computer` resolves correctly for user pages
- [ ] **SSL certificates**: Check cert validity and auto-renewal (Caddy should handle this)
- [ ] **@atproto/api version**: System uses `^0.18.0`, at/ experiments use `^0.17.0` — should unify

### P1 — Build

- [ ] **`ac-at` CLI** — Unified command-line tool consolidating all AT tooling (see section 7)
- [ ] **Lexicon publication** — Publish lexicon schemas to the network via DNS TXT + hosting
- [ ] **User pages deployment verification** — Confirm user-page.html is being served at subdomains

### P2 — Improve

- [ ] **Sync monitoring** — Automated checks for MongoDB ↔ PDS drift
- [ ] **Credential rotation** — PDS user passwords are stored in MongoDB; consider rotation strategy
- [ ] **Blob storage cleanup** — Orphaned blobs on PDS from deleted records
- [ ] **Rate limiting audit** — Current config: `PDS_RATE_LIMIT_ENABLED=true`, `PDS_MAX_ACCOUNTS=100`
- [ ] **Backup verification** — Confirm `backup.sh` runs and backups are restorable
- [ ] **Error handling** — Some sync functions silently continue on failure; add alerting

### P3 — Future

- [ ] **Federation** — Custom lexicon records are currently PDS-local; plan for relay/appview integration
- [ ] **OAuth/DPOP** — ATProto is moving toward OAuth; plan migration from password-based auth
- [ ] **Firehose consumer** — `firehose-monitor.mjs` exists but isn't integrated into monitoring
- [ ] **Cross-post from PDS to Bluesky** — Paintings/tapes could auto-post to app.bsky.feed.post

---

## 7. `ac-at` CLI Specification

Consolidate all AT tooling into a single CLI at `at/cli.mjs`, following the patterns of `memory/cli.mjs` and `papers/cli.mjs`.

### Commands

```
ac-at help                          Show all commands
ac-at health                        PDS health check (port from health-check.sh)
ac-at resolve <handle-or-did>       Resolve DID document
ac-at profile <handle-or-did>       Query profile information
ac-at posts <handle-or-did> [--limit=N]  Query posts
ac-at post <text> [--image=path]    Post to Bluesky
ac-at records <did> <collection> [--limit=N]  List records in collection
ac-at lexicons                      Show AC custom lexicons
ac-at invite                        Generate PDS invite code
ac-at accounts [--limit=N]          List PDS accounts
ac-at account:create <sub>          Create PDS account for AC user
ac-at account:check <handle-or-did> Inspect account status
ac-at sync:status                   Show sync stats across all collections
ac-at sync:paintings [--dry-run]    Backfill paintings to PDS
ac-at sync:moods [--dry-run]        Backfill moods to PDS
ac-at sync:verify <collection>      Compare MongoDB ↔ PDS for a collection
```

### npm scripts

```json
"at": "node at/cli.mjs",
"at:health": "node at/cli.mjs health",
"at:resolve": "node at/cli.mjs resolve",
"at:profile": "node at/cli.mjs profile",
"at:post": "node at/cli.mjs post",
"at:invite": "node at/cli.mjs invite"
```

---

## 8. Dependency Versions

| Package | system/ | at/ | Notes |
|---------|---------|-----|-------|
| `@atproto/api` | `^0.18.0` | `^0.17.0` | Should align to latest |
| `@atproto/identity` | — | `^0.4.3` | Only in experiments |
| `@atproto/lexicon` | — | `^0.4.2` | Only in experiments |
| `@atproto/syntax` | — | `^0.3.1` | Only in experiments |
| `@atproto/xrpc` | — | `^0.6.5` | Only in experiments |
| `dotenv` | (via netlify) | `^16.4.5` | CLI scripts need this |

---

## 9. Environment Variables

### Required for PDS operations

```
PDS_URL=https://at.aesthetic.computer     # PDS endpoint
PDS_ADMIN_PASSWORD=<secret>               # Admin API auth
```

### Required for Bluesky posting

```
BSKY_IDENTIFIER=aesthetic.computer        # Bluesky handle
BSKY_APP_PASSWORD=<app-password>          # From bsky.app/settings/app-passwords
BSKY_SERVICE=https://bsky.social          # Bluesky API endpoint
```

### PDS Server Config (on droplet)

```
PDS_HOSTNAME=pds.aesthetic.computer
PDS_ADMIN_EMAIL=me@jas.life
PDS_PORT=3000
PDS_BLOBSTORE_S3_BUCKET=aesthetic-pds-blobs
PDS_INVITE_REQUIRED=true
PDS_MAX_ACCOUNTS=100
```

---

## 10. File Map

```
at/
├── cli.mjs                          ← TO BUILD: unified CLI
├── package.json                     ← @atproto dependencies
├── lexicons/computer/aesthetic/     ← 6 lexicon JSON schemas
├── query-profile.mjs               ← standalone tools (to consolidate)
├── query-posts.mjs
├── post-to-bluesky.mjs
├── resolve-did.mjs
├── explore-lexicons.mjs
├── pds/
│   ├── config/pds.env.example
│   ├── scripts/                     ← 31 admin scripts
│   └── deployment/                  ← DigitalOcean provisioning
├── scripts/atproto/                 ← 28 sync/verify scripts (also at repo root)
├── user-page.html                   ← served at *.at.aesthetic.computer
└── deploy-user-pages.fish

system/backend/
├── at.mjs                           ← core PDS account management
├── media-atproto.mjs                ← unified record creation
├── painting-atproto.mjs             ← painting sync
├── mood-atproto.mjs                 ← mood sync
├── tape-atproto.mjs                 ← tape sync
└── news-atproto.mjs                 ← news sync

system/netlify/functions/
├── handle.mjs                       ← handle → PDS sync
├── mood.mjs                         ← mood creation + PDS
├── store-kidlisp.mjs                ← kidlisp + PDS
├── store-piece.mjs                  ← piece + PDS
├── tv.mjs / tv-tapes.mjs           ← query tapes from PDS
├── atproto-user-stats.mjs           ← stats aggregation
└── delete-erase-and-forget-me.mjs   ← cascade delete
```
