# `/at` Directory Manifest

**Purpose:** ATProto (Authenticated Transfer Protocol) experimentation and Bluesky integration toolkit for Aesthetic Computer.

**Created:** October 6, 2025  
**Status:** 🧪 Experimental - exploring before infrastructure commitment

---

## Directory Structure

```
/workspaces/aesthetic-computer/at/
├── README.md                  # Full documentation and learning resources
├── QUICKSTART.md             # Quick start guide with current state
├── MANIFEST.md               # This file - complete directory documentation
├── package.json              # Node.js project with ATProto dependencies
├── package-lock.json         # Locked dependency versions
├── .env.example              # Environment variable template
├── .env                      # Local config (gitignored, create from example)
├── .gitignore                # Git ignore patterns
├── node_modules/             # Installed dependencies (gitignored)
├── query-profile.mjs         # Query ATProto profiles
├── query-posts.mjs           # Fetch posts from feeds
├── post-to-bluesky.mjs       # Post to @aesthetic.computer account
├── explore-lexicons.mjs      # Browse lexicon schemas
├── resolve-did.mjs           # Resolve DIDs and inspect PDS
└── test-all.mjs              # Test suite for all tools
```

---

## Dependencies

### NPM Packages (from `package.json`)

| Package | Version | Purpose |
|---------|---------|---------|
| `@atproto/api` | ^0.17.0 | Main ATProto SDK for Bluesky |
| `@atproto/xrpc` | ^0.6.5 | Low-level XRPC client |
| `@atproto/lexicon` | ^0.4.2 | Lexicon schema tools |
| `@atproto/identity` | ^0.4.3 | DID resolution and handle verification |
| `@atproto/syntax` | ^0.3.1 | ATProto syntax utilities |
| `dotenv` | ^16.4.5 | Environment variable management |

**Total size:** ~23 packages when installed

---

## Tool Documentation

### 1. `query-profile.mjs`

**Purpose:** Query profile information for any ATProto handle or DID.

**Usage:**
```bash
node query-profile.mjs aesthetic.computer
node query-profile.mjs did:plc:k3k3wknzkcnekbnyde4dbatz
node query-profile.mjs bsky.app
```

**Output:**
- Handle and DID
- Display name and description
- Follower/following counts
- Post count
- Recent posts (last 5)
- Avatar and banner URLs

**API Used:** `https://public.api.bsky.app/xrpc/app.bsky.actor.getProfile`

**Authentication:** None required (public endpoint)

---

### 2. `query-posts.mjs`

**Purpose:** Fetch and display detailed post information from any account.

**Usage:**
```bash
node query-posts.mjs aesthetic.computer
node query-posts.mjs aesthetic.computer --limit=20
```

**Output:**
- Post text and timestamps
- Engagement metrics (likes, replies, reposts)
- Embeds (images, links, quoted posts)
- Rich text features (mentions, links, hashtags)
- Reply context

**API Used:** `https://public.api.bsky.app/xrpc/app.bsky.feed.getAuthorFeed`

**Authentication:** None required (public endpoint)

---

### 3. `post-to-bluesky.mjs`

**Purpose:** Post to the official @aesthetic.computer Bluesky account.

**Usage:**
```bash
# Simple text post
node post-to-bluesky.mjs "Hello from Aesthetic Computer! 🎨"

# Post with image
node post-to-bluesky.mjs "New painting!" --image=painting.png

# Post with image and alt text
node post-to-bluesky.mjs "Check this out" --image=art.png --alt="Abstract painting"
```

**Requirements:**
- `.env` file with credentials:
  ```env
  BSKY_IDENTIFIER=aesthetic.computer
  BSKY_APP_PASSWORD=your-app-password
  ```
- App password from: https://bsky.app/settings/app-passwords

**Output:**
- Post URI (AT-URI)
- Post CID (content identifier)
- Web URL to view on Bluesky

**API Used:** `https://bsky.social` (login + create post)

**Authentication:** Required (OAuth with app password)

---

### 4. `explore-lexicons.mjs`

**Purpose:** Browse and understand ATProto lexicon schemas.

**Usage:**
```bash
node explore-lexicons.mjs
```

**Output:**
- Standard Bluesky lexicons (`app.bsky.*`)
  - `app.bsky.feed.post` - Posts/status updates
  - `app.bsky.feed.like` - Likes on posts
  - `app.bsky.graph.follow` - Follow relationships
  - `app.bsky.embed.images` - Image embeds
  - `app.bsky.embed.external` - Link embeds
  
- Proposed AC custom lexicons (`computer.aesthetic.*`)
  - `computer.aesthetic.painting` - Paintings with rich metadata
  - `computer.aesthetic.piece` - Interactive pieces/programs
  - `computer.aesthetic.kidlisp` - KidLisp code snippets
  - `computer.aesthetic.mood` - Mood/emotion entries

**Purpose:** Educational tool to understand federation and lexicon design

---

### 5. `resolve-did.mjs`

**Purpose:** Resolve DIDs to inspect PDS configurations and DID documents.

**Usage:**
```bash
node resolve-did.mjs aesthetic.computer
node resolve-did.mjs did:plc:k3k3wknzkcnekbnyde4dbatz
node resolve-did.mjs bsky.app
```

**Output:**
- DID identifier
- Verified handles (alsoKnownAs)
- PDS endpoint (where data is hosted)
- Verification methods (public keys)
- Full DID document (JSON)

**API Used:** `https://plc.directory/{did}` for `did:plc:*` DIDs

**Authentication:** None required (public PLC directory)

---

### 6. `test-all.mjs`

**Purpose:** Quick test suite to verify all tools are working.

**Usage:**
```bash
node test-all.mjs
```

**Tests:**
1. ✅ Profile query
2. ✅ Feed query  
3. ⏭️ Authentication (if credentials provided)

**Output:** Pass/fail status for each test with helpful error messages.

---

## Current State

### @aesthetic.computer on Bluesky

```json
{
  "handle": "aesthetic.computer",
  "did": "did:plc:k3k3wknzkcnekbnyde4dbatz",
  "pds": "https://chanterelle.us-west.host.bsky.network",
  "followers": 0,
  "following": 2,
  "posts": 1,
  "created": "2024-10-20T01:54:27Z",
  "lastPost": "ty the invite 🦋"
}
```

**PDS Provider:** Bluesky (third-party infrastructure)

**Handle Verification:** ✅ Custom domain `aesthetic.computer` verified via DNS

---

## Environment Setup

### Required Environment Variables

Create `.env` file (from `.env.example`):

```env
# Bluesky Service (default: official PDS)
BSKY_SERVICE=https://bsky.social

# For posting to @aesthetic.computer (optional)
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=your-app-password-here

# For firehose monitoring (future)
BSKY_RELAY=wss://bsky.network
```

### Getting App Password

1. Log in to https://bsky.app
2. Go to Settings → App Passwords
3. Create new app password
4. Name it "Aesthetic Computer Dev" or similar
5. Copy password to `.env`

**Security:** App passwords are scoped - they cannot change account settings or delete the account.

---

## Integration Paths

### Path 1: Simple Integration (Recommended First Step)

**Goal:** Share paintings to @aesthetic.computer Bluesky account

**Implementation:**
```javascript
// In AC's painting share handler
import { BskyAgent } from '@atproto/api'

async function sharePaintingToBluesky(painting, user) {
  const agent = new BskyAgent({ service: 'https://bsky.social' })
  await agent.login({
    identifier: process.env.BSKY_IDENTIFIER,
    password: process.env.BSKY_APP_PASSWORD
  })

  // Upload painting image
  const imageData = await fetch(painting.url).then(r => r.arrayBuffer())
  const { data } = await agent.uploadBlob(
    new Uint8Array(imageData),
    { encoding: 'image/png' }
  )

  // Post to Bluesky
  await agent.post({
    text: `New painting by @${user.handle}: "${painting.title}"\n\n🎨 aesthetic.computer/${painting.slug}`,
    embed: {
      $type: 'app.bsky.embed.images',
      images: [{
        image: data.blob,
        alt: painting.description || painting.title
      }]
    }
  })
}
```

**Benefits:**
- ✅ Zero infrastructure
- ✅ Instant Bluesky presence
- ✅ Marketing and showcase
- ✅ Test federation
- ✅ Learn ATProto

**Drawbacks:**
- ❌ No per-user identities
- ❌ All posts from one account
- ❌ No true data portability

---

### Path 2: Full PDS Deployment (Future)

**Goal:** Run AC's own PDS with per-user identities

**Details:** See [ATProto PDS Roadmap](../plans/atproto-pds-roadmap.md)

**Features:**
- ✅ Per-user identities (`jeffrey.aesthetic.computer`)
- ✅ Custom lexicons (`computer.aesthetic.*`)
- ✅ True data portability
- ✅ Full federation
- ✅ Replace Auth0

**Requirements:**
- GCP Compute Engine e2-small (~$15-20/month)
- PostgreSQL database
- DNS configuration
- PDS deployment and maintenance

---

## Development Workflow

### Testing Changes

```bash
# 1. Make changes to tools
vim query-profile.mjs

# 2. Run tests
node test-all.mjs

# 3. Test specific tool
node query-profile.mjs aesthetic.computer

# 4. Check for lint errors
npm run test
```

### Adding New Tools

1. Create new `.mjs` file in `/at`
2. Add to `package.json` scripts section
3. Document in this MANIFEST
4. Update README.md
5. Test with `test-all.mjs`

### Updating Dependencies

```bash
# Check for updates
npm outdated

# Update all to latest
npm update

# Update specific package
npm install @atproto/api@latest
```

---

## Related Documentation

- **Main README:** [`/at/README.md`](README.md) - Full documentation
- **Quick Start:** [`/at/QUICKSTART.md`](QUICKSTART.md) - Get started quickly
- **PDS Roadmap:** [`/plans/atproto-pds-roadmap.md`](../plans/atproto-pds-roadmap.md) - Full PDS strategy
- **Data Architecture:** [`/plans/atproto-data-architecture.md`](../plans/atproto-data-architecture.md) - Data flow and storage

---

## External Resources

- **ATProto Docs:** https://atproto.com
- **Bluesky API:** https://docs.bsky.app
- **ATProto GitHub:** https://github.com/bluesky-social/atproto
- **SDK Reference:** https://github.com/bluesky-social/atproto/tree/main/packages/api
- **Lexicon Specs:** https://atproto.com/specs/lexicon
- **PLC Directory:** https://plc.directory
- **Bluesky Settings:** https://bsky.app/settings

---

## Docker Integration

### Dockerfile Changes

ATProto dependencies are now included in `.devcontainer/Dockerfile`:

```dockerfile
# --- ATProto dependencies for /at directory ------------------------------------
# Note: These will be available globally for ATProto experiments
# See /at directory for ATProto/Bluesky exploration tools
RUN export PATH="/home/me/.fnm:${PATH}" && \
    eval "$(/home/me/.fnm/fnm env --use-on-cd)" && \
    npm i -g @atproto/api@^0.17.0 @atproto/xrpc@^0.6.5 @atproto/lexicon@^0.4.2 @atproto/identity@^0.4.3 @atproto/syntax@^0.3.1 dotenv@^16.4.5
```

This ensures all dependencies are available after container rebuilds.

---

## Future Enhancements

### Planned Tools

- [ ] `firehose-monitor.mjs` - Monitor ATProto firehose in real-time
- [ ] `create-lexicon.mjs` - Generate custom lexicon schemas
- [ ] `export-data.mjs` - Export all data from a DID as CAR files
- [ ] `migrate-to-pds.mjs` - Migration tool for when AC runs own PDS
- [ ] `analyze-network.mjs` - Network analysis and metrics

### Planned Features

- [ ] Batch posting support
- [ ] Image optimization before upload
- [ ] Post scheduling
- [ ] Analytics and engagement tracking
- [ ] Custom lexicon validator
- [ ] PDS health monitoring

---

## Changelog

### 2025-10-06 - Initial Creation

- ✅ Created `/at` directory structure
- ✅ Set up Node.js project with ATProto dependencies
- ✅ Implemented 6 core tools (query, post, explore, resolve, test)
- ✅ Documented intermediate integration path
- ✅ Added to main README and Dockerfile
- ✅ Tested against @aesthetic.computer Bluesky account
- ✅ Created comprehensive documentation

**Status:** Ready for experimentation and integration testing

---

**Maintainer:** Jeffrey Scudder (@jeffrey)  
**Last Updated:** October 6, 2025  
**Version:** 0.0.1 (Experimental)
