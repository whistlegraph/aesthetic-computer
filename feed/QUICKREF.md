# Quick Reference - Feed System

## Active Channel

**Channel ID**: `23b63744-649f-4274-add5-d1b439984e51`

**URL**: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51

## Quick Commands

### Build All Playlists
```bash
cd /workspaces/aesthetic-computer/feed
./build-feed.fish
```

### Generate Individual Playlists
```bash
# Load environment first
set -gx (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#' | string split '=')

# Top 100 KidLisp
node create-top-kidlisp-playlist.mjs

# Colors
node create-kidlisp-colors-playlist.mjs

# Chords
node create-kidlisp-chords-playlist.mjs
```

### Check Channel Status
```bash
curl -s "https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51" \
  -H "Authorization: Bearer $FEED_API_SECRET" | jq
```

### List All Playlists
```bash
curl -s "https://feed.aesthetic.computer/api/v1/playlists" \
  -H "Authorization: Bearer $FEED_API_SECRET" | jq '.items[] | "\(.id) | \(.title)"'
```

### Delete a Playlist
```bash
curl -X DELETE "https://feed.aesthetic.computer/api/v1/playlists/PLAYLIST_ID" \
  -H "Authorization: Bearer $FEED_API_SECRET"
```

## Current Playlists

| Title | ID | Items | Generator Script |
|-------|-----|-------|------------------|
| Top 100 as of Monday, October 6, 2025 | f60493a2-9e69-4e6b-837e-76047f48438c | 100 | create-top-kidlisp-playlist.mjs |
| Colors | 2680b102-04ee-47b5-b7d7-f814094695e7 | 151 | create-kidlisp-colors-playlist.mjs |
| Chords for `clock` | e1bf1aae-2427-4dd0-a39d-f5da89fdf02e | 31 | create-kidlisp-chords-playlist.mjs |

## Environment Variables

All secrets are stored in `../aesthetic-computer-vault/feed/.env`:

- `FEED_API_SECRET` - API auth token
- `MONGODB_CONNECTION_STRING` - MongoDB connection
- `MONGODB_NAME` - Database name (aesthetic)
- `KIDLISP_CHANNEL_ID` - Active channel ID

These are automatically loaded in devcontainer startup via entry.fish.

## Deployment

The Cloudflare Worker is in the `dp1-feed` directory (cloned from https://github.com/feral-file/dp1-feed).

### Deploy Worker
```bash
cd /workspaces/aesthetic-computer/dp1-feed
wrangler deploy
```

### Update Worker Secrets
```bash
wrangler secret put API_SECRET
wrangler secret put ED25519_PRIVATE_KEY
```

## Useful Links

- Feed API Base: https://feed.aesthetic.computer/api/v1
- DP-1 Spec: https://github.com/feral-file/dp1-feed
- MongoDB: https://cloud.mongodb.com/
- Cloudflare Dashboard: https://dash.cloudflare.com
