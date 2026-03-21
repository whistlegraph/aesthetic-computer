# MongoDB Migration Report: aesthetic-computer

**Date:** 2026-02-10
**Status:** Database backup completed, analysis complete
**Decision:** Self-host MongoDB on DigitalOcean as **silo.aesthetic.computer** -- a unified data & storage dashboard

---

## 1. Current State

### MongoDB Atlas Cluster
- **Cluster:** `aesthetic.qencn.mongodb.net` (Atlas shared/free tier)
- **Database:** `aesthetic`
- **Driver:** Native MongoDB Node.js driver (no Mongoose/ODM)
- **Collections:** 38
- **Total documents:** ~128,000
- **Backup size:** 150MB JSON (115MB is boot logs alone)

### What's Actually Stored

| Category | Collections | Docs | Notes |
|----------|------------|------|-------|
| **Logs/Telemetry** | boots, logs, bundle-telemetry, oven-grabs, oven-bakes, oven-cache | ~60,225 | 47% of all docs. Disposable. |
| **Chat** | chat-system, chat-clock, chat-sotce, + 3 mute tables | ~31,612 | 25% of docs. Append-only messages. |
| **User Content** | kidlisp, paintings, pieces, moods, tapes, clocks | ~22,083 | 17% of docs. The core creative data. |
| **Users/Auth** | users, @handles, verifications, secrets | ~10,872 | 8.5% of docs. Identity/auth data. |
| **Social/Misc** | sotce-*, news-*, piece-hits, tickets, products, etc. | ~4,284 | 3.5%. Small feature tables. |

### Key Observation

This is a **small database**. 128k documents, 150MB total, with almost half being disposable logs. The actual meaningful data (users, content, chat) is well under 50MB. MongoDB Atlas is overkill for this workload.

---

## 2. Access Patterns Analysis

### Reads (~85% of operations)
- `findOne()` by `_id` or unique key (handle lookup, user lookup, painting by code)
- `find().sort().limit()` (chat history, recent moods, latest paintings)
- `aggregate()` with `$lookup` (moods joined with handles, stats)
- `countDocuments()` and `distinct()` (metrics, user lists)

### Writes (~15% of operations)
- `insertOne()` (new chat message, new painting, new mood)
- `updateOne()` with `$set` or `$inc` (update profile, increment hits)
- `deleteMany()` (account deletion - paintings, pieces, moods for a user)

### Critical Finding
- **No transactions used** -- all operations are single-document
- **No change streams or watch cursors**
- **No GridFS** -- binary files are in DigitalOcean Spaces
- **Aggregation pipelines** are simple (1-2 stage lookups, not MapReduce)
- **Indexes** are straightforward single/compound keys
- **Already using Redis** for caching hot paths (chat, handles, sessions)

This means: **you don't need MongoDB's document model advantages**. Your data is effectively relational with simple key-value lookups.

---

## 3. Recommendation: Turso (LibSQL/SQLite)

### Why Turso

| Factor | MongoDB Atlas | Turso |
|--------|--------------|-------|
| **Monthly cost** | ~$25-57/mo (M0 free tier suspended = M2+ needed) | **$0** (free tier: 500 DBs, 9GB storage, 25M reads/mo) |
| **Data model fit** | Document store (unused flexibility) | Relational (matches your actual patterns) |
| **Serverless** | Cold start connection issues (your 3-strategy fallback code) | Edge-native, embedded replica, no cold starts |
| **Hosting** | Atlas cloud (external dependency) | Embedded + cloud sync (works offline too) |
| **Driver** | `mongodb` npm (heavy) | `@libsql/client` npm (lightweight) |
| **Backup** | Atlas-managed (opaque) | SQLite file (just copy it) |
| **Netlify Functions** | Connection pooling headaches | Per-request, no pooling needed |

### Why NOT Postgres/Supabase/PlanetScale
- **Supabase (Postgres):** Great but heavier than needed. Your data is 50MB. A full Postgres cluster is like driving a semi truck to pick up groceries.
- **PlanetScale:** MySQL-based, good but recently removed free tier. Similar overkill.
- **Neon (Postgres):** Serverless Postgres, decent option, but more complex than needed.
- **Turso** is literally SQLite over HTTP with edge replicas -- the lightest possible real database that still gives you SQL, joins, and ACID.

### Why NOT just JSON files / KV store
- You use `$lookup` joins (moods + handles), sorted queries, unique indexes, aggregations
- You need ACID for user deletion (delete across multiple tables)
- A real database is warranted -- just not a *heavy* one

---

## 4. Migration Schema Design

```sql
-- Core identity
CREATE TABLE users (
  id TEXT PRIMARY KEY,          -- auth0 sub (e.g. "auth0|abc123")
  code TEXT UNIQUE,
  email TEXT,
  stripe_id TEXT,
  tezos_address TEXT,
  atproto_did TEXT,
  atproto_handle TEXT,
  created_at TEXT,
  data TEXT                     -- JSON blob for flexible fields
);

CREATE TABLE handles (
  user_id TEXT PRIMARY KEY REFERENCES users(id),
  handle TEXT UNIQUE NOT NULL
);

CREATE TABLE verifications (
  user_id TEXT PRIMARY KEY REFERENCES users(id),
  verified INTEGER DEFAULT 0,
  data TEXT
);

-- Creative content
CREATE TABLE paintings (
  id TEXT PRIMARY KEY,
  slug TEXT NOT NULL,
  code TEXT UNIQUE,
  user_id TEXT REFERENCES users(id),
  hash TEXT,
  bucket TEXT,
  nuked INTEGER DEFAULT 0,
  created_at TEXT,
  UNIQUE(slug, user_id)
);

CREATE TABLE pieces (
  id TEXT PRIMARY KEY,
  slug TEXT NOT NULL,
  user_id TEXT REFERENCES users(id),
  created_at TEXT,
  UNIQUE(slug, user_id)
);

CREATE TABLE kidlisp (
  id TEXT PRIMARY KEY,
  code TEXT UNIQUE,
  hash TEXT UNIQUE,
  user_id TEXT,
  text TEXT,
  hits INTEGER DEFAULT 0,
  kept_network TEXT,
  created_at TEXT,
  data TEXT                     -- JSON for atproto rkey, mint status, etc.
);

CREATE TABLE moods (
  id TEXT PRIMARY KEY,
  user_id TEXT REFERENCES users(id),
  mood TEXT,
  deleted INTEGER DEFAULT 0,
  atproto_rkey TEXT,
  bluesky_uri TEXT,
  created_at TEXT
);

CREATE TABLE tapes (
  id TEXT PRIMARY KEY,
  code TEXT UNIQUE,
  slug TEXT,
  user_id TEXT,
  created_at TEXT,
  data TEXT
);

CREATE TABLE clocks (
  id TEXT PRIMARY KEY,
  code TEXT UNIQUE,
  hash TEXT UNIQUE,
  user_id TEXT,
  hits INTEGER DEFAULT 0,
  created_at TEXT,
  data TEXT
);

-- Chat (one table, partitioned by instance name)
CREATE TABLE chat_messages (
  id TEXT PRIMARY KEY,
  instance TEXT NOT NULL,       -- 'system', 'sotce', 'clock'
  user_id TEXT,
  text TEXT,
  font TEXT,
  created_at TEXT
);
CREATE INDEX idx_chat_instance_when ON chat_messages(instance, created_at);

CREATE TABLE chat_mutes (
  user_id TEXT NOT NULL,
  instance TEXT NOT NULL,
  PRIMARY KEY(user_id, instance)
);

-- Social
CREATE TABLE news_posts (
  id TEXT PRIMARY KEY,
  code TEXT UNIQUE,
  user_id TEXT,
  title TEXT,
  url TEXT,
  text TEXT,
  score INTEGER DEFAULT 0,
  status TEXT DEFAULT 'active',
  created_at TEXT
);

CREATE TABLE news_comments (
  id TEXT PRIMARY KEY,
  post_code TEXT REFERENCES news_posts(code),
  parent_id TEXT,
  user_id TEXT,
  text TEXT,
  score INTEGER DEFAULT 0,
  status TEXT DEFAULT 'active',
  created_at TEXT
);

CREATE TABLE news_votes (
  item_type TEXT NOT NULL,
  item_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  created_at TEXT,
  PRIMARY KEY(item_type, item_id, user_id)
);

-- Analytics (consider just dropping these or using a log file)
CREATE TABLE piece_hits (
  piece TEXT PRIMARY KEY,
  hits INTEGER DEFAULT 0
);

CREATE TABLE piece_user_hits (
  piece TEXT NOT NULL,
  user_id TEXT NOT NULL,
  hits INTEGER DEFAULT 0,
  PRIMARY KEY(piece, user_id)
);

-- Misc
CREATE TABLE products (id TEXT PRIMARY KEY, data TEXT);
CREATE TABLE tickets (id TEXT PRIMARY KEY, data TEXT);
CREATE TABLE secrets (id TEXT PRIMARY KEY, data TEXT);
CREATE TABLE servers (id TEXT PRIMARY KEY, data TEXT);

-- SOTCE game
CREATE TABLE sotce_pages (id TEXT PRIMARY KEY, user_id TEXT, state TEXT, data TEXT);
CREATE TABLE sotce_touches (id TEXT PRIMARY KEY, user_id TEXT, page TEXT, data TEXT, UNIQUE(user_id, page));
CREATE TABLE sotce_asks (id TEXT PRIMARY KEY, data TEXT);
```

### Collections to DROP (not migrate)
- `boots` (25,513 docs, 115MB) -- startup logs, no value
- `oven-grabs` (30,560 docs, 15MB) -- screenshot cache metadata
- `oven-cache` (1,362 docs) -- ephemeral cache
- `oven-bakes` (30 docs) -- process logs
- `oven-frozen-pieces` (307 docs) -- can regenerate
- `bundle-telemetry` (440 docs) -- build metrics, disposable
- `logs` (2,320 docs) -- old chat/system logs
- `false.work-builds` (23 docs) -- game build records (archive to JSON)
- `false.work-reports` (2 docs) -- archive to JSON
- `jas-tags` (10 docs) -- archive to JSON
- `jas-stretched-paintings` (1 doc) -- archive to JSON

**That eliminates ~60,000 documents (47% of the DB) and 130MB (87% of backup size).**

---

## 5. Chosen Direction: silo.aesthetic.computer

### What is Silo?

Self-hosted MongoDB on a DigitalOcean droplet, plus a master data dashboard that provides visibility across all of AC's data and storage. Not just a database -- the service that *knows everything* about AC's data.

```
silo.aesthetic.computer ($6/mo DO Droplet)
├── mongod                              (self-hosted MongoDB)
├── Fastify API + dashboard             (index.html, AC house style)
│   ├── /                               (master data dashboard)
│   ├── /db                             (mongo-express for direct DB browsing)
│   ├── /api/storage                    (Spaces bucket stats)
│   │   ├── GET /api/storage/usage              → total GB across all buckets
│   │   ├── GET /api/storage/buckets            → per-bucket breakdown
│   │   ├── GET /api/storage/user/:id           → per-user storage usage
│   │   └── GET /api/storage/top-users          → biggest storage consumers
│   ├── /api/db                         (database stats)
│   │   ├── GET /api/db/collections             → doc counts, sizes
│   │   ├── GET /api/db/users/count             → total registered users
│   │   └── GET /api/db/health                  → mongod status + uptime
│   ├── /api/services                   (other AC service status)
│   │   ├── GET /api/services/oven              → oven health + recent bakes
│   │   ├── GET /api/services/session           → session server connections
│   │   └── GET /api/services/atproto           → ATProto sync status
│   ├── /api/kidlisp                    (kidlisp keeps overview)
│   │   └── GET /api/kidlisp/stats              → total keeps, minted, recent
│   └── /api/overview                   (unified platform stats -- one call)
├── Caddy                               (HTTPS + reverse proxy)
└── cron: mongodump → DO Spaces         (nightly automated backup)
```

### Why Self-Host Mongo (Not Switch Databases)

1. **Zero code changes** -- just swap the connection string in .env files
2. **No new vendor dependency** -- runs on infrastructure we already own
3. **Dashboard as bonus** -- Atlas gave us nothing to look at; silo gives us full visibility
4. **Cost: $6/mo vs $25-57/mo** -- immediate savings
5. **Decouples cost from architecture** -- can still explore SQLite/Postgres later from a position of clarity, not pressure

### Migration Steps

1. Provision DO droplet, install MongoDB Community Edition
2. Import from JSON backup (already in `mongodb-backup/`)
3. Set up Caddy for HTTPS on `silo.aesthetic.computer`
4. Build the dashboard (`silo/index.html`, AC house style)
5. Set up nightly `mongodump` to DigitalOcean Spaces
6. Update connection strings in all .env files
7. Verify all services connect, then decommission Atlas

### Estimated Savings

| Item | Current | After Migration |
|------|---------|----------------|
| MongoDB Atlas | $25-57/mo | $0 (eliminated) |
| Silo droplet | $0 | $6/mo |
| Code changes | -- | Zero (connection string swap) |
| Bonus | No visibility | Full data dashboard |

**Net savings: ~$19-51/month**, zero refactoring risk, plus a master dashboard we never had.

### Future Optionality

Silo doesn't lock us into MongoDB forever. Once running, we can:
- Explore adding SQLite alongside Mongo for specific use cases
- Introduce `world_id` when instanceability actually matters
- Migrate individual collections to other storage if beneficial
- The dashboard itself becomes the observatory for making these decisions with real data

---

## 6. Backup Location

Full database backup saved to:
```
/workspaces/aesthetic-computer/mongodb-backup/
├── _manifest.json          (collection inventory + indexes)
├── @handles.json           (2,773 docs)
├── boots.json              (25,513 docs - 115MB)
├── chat-system.json        (17,853 docs)
├── chat-clock.json         (13,465 docs)
├── kidlisp.json            (14,189 docs)
├── moods.json              (2,874 docs)
├── paintings.json          (4,358 docs)
├── users.json              (4,217 docs)
├── verifications.json      (3,876 docs)
├── ... (38 collection files total)
└── Total: 150MB
```

**Add `mongodb-backup/` to `.gitignore` -- it contains user data and secrets.**

---

## 8. Decision Matrix

| Option | Cost/mo | Code Changes | Vendor Risk | Fit for AC |
|--------|---------|-------------|-------------|------------|
| **Self-host Mongo as silo (chosen)** | **$6** | **None** | **None** | **Best -- zero risk, full control, dashboard** |
| Turso (LibSQL) | $0 | ~50 files | New vendor | Good tech, but another company |
| Supabase (Postgres) | $0-25 | ~50 files | New vendor | Heavier than needed |
| Keep MongoDB Atlas (upgrade) | $25-57+ | None | Atlas suspension risk | Expensive for 50MB |
| SQLite on session server | $0 | ~50 files | None | Good but mixes concerns |

---

## 9. Next Steps

1. **Done:** Backup complete, `.gitignore` updated, reports written.
2. **Next:** Provision DO droplet, install MongoDB, import data.
3. **Then:** Build silo dashboard (AC house style -- see [style guide](ac-html-frontend-style-guide.md)).
4. **Then:** Set up Caddy + automated Spaces backups.
5. **Then:** Swap connection strings, verify all services, decommission Atlas.
