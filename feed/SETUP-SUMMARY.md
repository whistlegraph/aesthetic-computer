# Feed System Setup Summary

## Overview

The Aesthetic Computer Feed system is now fully set up and operational at `feed.aesthetic.computer`.

## Directory Structure

```
/workspaces/aesthetic-computer/
├── feed/                              # Main feed scripts and documentation
│   ├── README.md                      # Comprehensive documentation
│   ├── QUICKREF.md                    # Quick reference commands
│   ├── DEPLOYMENT.md                  # Worker deployment guide
│   ├── channel-config.json            # Active channel configuration
│   ├── build-feed.fish                # Main build script (loads vault secrets)
│   ├── build-kidlisp-feed.mjs         # Automated build & cleanup script
│   ├── create-top-kidlisp-playlist.mjs    # Generate Top 100 playlist
│   ├── create-kidlisp-colors-playlist.mjs # Generate Colors playlist
│   └── create-kidlisp-chords-playlist.mjs # Generate Chords playlist
│
├── dp1-feed/                          # Cloudflare Worker (cloned from GitHub)
│   ├── wrangler.toml                  # Worker configuration
│   ├── src/                           # TypeScript source code
│   └── package.json                   # Dependencies
│
└── aesthetic-computer-vault/feed/     # Private secrets (git-ignored)
    ├── .env                           # Environment variables & secrets
    └── README.md                      # Secrets documentation
```

## Active Resources

### Channel
- **ID**: `23b63744-649f-4274-add5-d1b439984e51`
- **URL**: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51
- **Title**: KidLisp
- **Curator**: prompt.ac

### Playlists (as of October 6, 2025)

1. **Top 100 as of Monday, October 6, 2025**
   - ID: `f60493a2-9e69-4e6b-837e-76047f48438c`
   - Items: 100 most-hit KidLisp pieces
   - Source: MongoDB `aesthetic.kidlisp` collection

2. **Colors**
   - ID: `2680b102-04ee-47b5-b7d7-f814094695e7`
   - Items: 151 CSS color names
   - Source: `system/public/aesthetic.computer/lib/num.mjs`

3. **Chords for `clock`**
   - ID: `e1bf1aae-2427-4dd0-a39d-f5da89fdf02e`
   - Items: 31 Western musical chords
   - Source: clock piece with notepat notation

## Environment Setup

### DevContainer Auto-Setup

The devcontainer automatically loads feed secrets on startup via entry.fish:
1. Checks if vault exists at `aesthetic-computer-vault/feed/.env`
2. Copies to `~/envs/feed.env` for persistence
3. Exports all variables globally

### Manual Setup

```fish
# Load secrets manually
set -gx (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#' | string split '=')
```

## Building the Feed

### Quick Build (Recommended)

```bash
cd /workspaces/aesthetic-computer/feed
./build-feed.fish
```

### Manual Build

```bash
cd /workspaces/aesthetic-computer/feed
node build-kidlisp-feed.mjs
```

## Secrets Management

All secrets are stored in `aesthetic-computer-vault/feed/.env`:
- `FEED_API_SECRET` - API authentication
- `MONGODB_CONNECTION_STRING` - MongoDB connection
- `MONGODB_NAME` - Database name
- `KIDLISP_CHANNEL_ID` - Active channel ID

⚠️ **Never commit these secrets to public repositories!**

## Links

- Feed API: https://feed.aesthetic.computer/api/v1
- Active Channel: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51
- DP-1 Spec: https://github.com/feral-file/dp1-feed

---

✅ **System Status**: Fully operational
📅 **Last Updated**: October 6, 2025
