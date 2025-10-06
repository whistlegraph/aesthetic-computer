# Aesthetic Computer Feed System

This directory contains the DP-1 Feed system for Aesthetic Computer, deployed to `feed.aesthetic.computer`.

## Overview

The Feed system provides curated content channels for the Aesthetic Computer platform, using the DP-1 (Distributed Playlist) specification. It's built on Cloudflare Workers with KV storage and queue-based asynchronous operations.

## Architecture

- **Worker**: Cloudflare Worker deployed to `feed.aesthetic.computer/api/v1`
- **Source**: [feral-file/dp1-feed](https://github.com/feral-file/dp1-feed)
- **Framework**: TypeScript + Hono
- **Storage**: 3 Cloudflare KV Namespaces
  - `DP1_PLAYLISTS` - Stores playlist data
  - `DP1_CHANNELS` - Stores channel data
  - `DP1_PLAYLIST_ITEMS` - Stores individual playlist items
- **Queue**: `dp1-write-operations-prod` - Handles async write operations

## Active Channel

**KidLisp Channel**
- Channel ID: `23b63744-649f-4274-add5-d1b439984e51`
- URL: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51
- Title: "KidLisp"
- Curator: "prompt.ac"
- Description: "KidLisp is a TV friendly programming language for kids of all ages! Developed by @jeffrey of Aesthetic Computer."

### Playlists

1. **Top 100 as of [date]** - 100 most-hit KidLisp pieces from MongoDB
2. **Colors** - 151 CSS color names as solid color displays
3. **Chords for `clock`** - 31 Western musical chords using the clock piece notation

All playlists use TV-friendly parameters: `?tv=true&density=5`

## Scripts

### Build Script
```bash
cd /workspaces/aesthetic-computer/feed
MONGODB_CONNECTION_STRING="..." \
MONGODB_NAME="aesthetic" \
FEED_API_SECRET="..." \
node build-kidlisp-feed.mjs
```

The build script:
1. Generates Top 100 playlist from MongoDB kidlisp collection
2. Generates Colors playlist from CSS color names
3. Generates Chords playlist using clock piece notation
4. Updates the KidLisp channel with all 3 playlists
5. Cleans up stale/orphaned playlists
6. Displays final feed structure

### Individual Playlist Scripts

```bash
# Top 100 KidLisp pieces
node create-top-kidlisp-playlist.mjs

# CSS Colors
node create-kidlisp-colors-playlist.mjs

# Musical Chords
node create-kidlisp-chords-playlist.mjs
```

## Environment Variables

Required environment variables (stored in aesthetic-computer-vault):

- `FEED_API_SECRET` - API authentication token for feed.aesthetic.computer
- `MONGODB_CONNECTION_STRING` - MongoDB connection string for aesthetic database
- `MONGODB_NAME` - MongoDB database name (usually "aesthetic")

See `../aesthetic-computer-vault/feed/.env` for actual values.

## Deployment

The Cloudflare Worker is deployed using wrangler:

```bash
cd dp1-feed
wrangler deploy
```

Configuration is in `dp1-feed/wrangler.toml`.

### Secrets

Set worker secrets using wrangler:

```bash
wrangler secret put API_SECRET
wrangler secret put ED25519_PRIVATE_KEY
```

### KV Namespaces

Production KV namespace IDs (configured in wrangler.toml):
- `DP1_PLAYLISTS`: (ID in wrangler.toml)
- `DP1_CHANNELS`: (ID in wrangler.toml)
- `DP1_PLAYLIST_ITEMS`: (ID in wrangler.toml)

### Queue

Production queue: `dp1-write-operations-prod`

## API Endpoints

Base URL: `https://feed.aesthetic.computer/api/v1`

### Channels
- `GET /channels` - List all channels
- `GET /channels/:id` - Get channel details
- `POST /channels` - Create new channel
- `PATCH /channels/:id` - Update channel
- `PUT /channels/:id` - Replace channel

### Playlists
- `GET /playlists` - List all playlists
- `GET /playlists/:id` - Get playlist details
- `POST /playlists` - Create new playlist
- `DELETE /playlists/:id` - Delete playlist

### Authentication

All write operations require Bearer token authentication:

```bash
curl -H "Authorization: Bearer $FEED_API_SECRET" ...
```

## DP-1 Specification

Playlists follow the DP-1 (Distributed Playlist) v1.0.0 specification:

```javascript
{
  "dpVersion": "1.0.0",
  "title": "Playlist Title",
  "description": "Optional description",
  "license": "open",
  "items": [
    {
      "source": "https://aesthetic.computer/piece?params",
      "url": "/piece",
      "title": "Item Title",
      "creator": "Creator Name",
      "duration": 24,
      "license": "open"
    }
  ]
}
```

## Data Sources

### MongoDB (KidLisp Collection)
- Connection: `aesthetic.qencn.mongodb.net`
- Database: `aesthetic`
- Collection: `kidlisp`
- Schema: `{code, source, hash, when, hits, lastAccessed, user, handle}`

### CSS Colors
151 color names from `system/public/aesthetic.computer/lib/num.mjs` (lines 604-753)

### Musical Chords
31 Western chords using clock piece's notepat notation:
- Sharps: v=C#, s=D#, w=F#, r=G#, q=A#
- Next octave: h,i,j,k,l,m,n
- Struck notes: `clock~^[notes]` where ^ enables chord mode

## Development

### Setup

1. Clone dp1-feed repository (if not present):
```bash
git clone https://github.com/feral-file/dp1-feed.git
cd dp1-feed
npm install
```

2. Copy environment variables from vault:
```bash
cp ../aesthetic-computer-vault/feed/.env .env
```

3. Configure wrangler with Cloudflare credentials

4. Run build script to update feed:
```bash
cd /workspaces/aesthetic-computer/feed
./build-feed.sh
```

### Adding New Playlists

1. Create a new script: `create-[name]-playlist.mjs`
2. Follow DP-1 specification format
3. Upload to feed API using Bearer auth
4. Update channel with new playlist ID
5. Add to `build-kidlisp-feed.mjs` to include in automated builds

### Cleaning Up

The build script automatically cleans up stale playlists. To manually delete:

```bash
curl -X DELETE "https://feed.aesthetic.computer/api/v1/playlists/:id" \
  -H "Authorization: Bearer $FEED_API_SECRET"
```

Note: Channels cannot be deleted via API (by design).

## Troubleshooting

### Common Issues

**MongoDB connection errors**: Ensure `MONGODB_CONNECTION_STRING` includes proper auth and uses MongoDB Atlas connection string format.

**Playlist validation errors**: Check that all required DP-1 fields are present:
- `dpVersion`: "1.0.0"
- `license`: "open" (must match exact enum value)
- `duration`: numeric value in seconds
- `items`: array of content items

**Channel update 500 errors**: These often resolve on retry. The API may have caching/timing issues. Wait a few seconds and try again.

**Storage errors on delete**: Some deletes fail with `storage_error` but eventually succeed. Retry or check if resource is already deleted.

## Links

- Feed API: https://feed.aesthetic.computer/api/v1
- KidLisp Channel: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51
- DP-1 Spec: https://github.com/feral-file/dp1-feed
- Cloudflare Dashboard: https://dash.cloudflare.com
