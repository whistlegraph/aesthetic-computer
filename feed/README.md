# Feed System Secrets

This directory contains sensitive configuration for the Aesthetic Computer Feed system.

## Files

- `.env` - Environment variables for feed scripts and deployment
- `ed25519-private-key.txt` - ED25519 private key for DP-1 playlist signing (if needed)

## Usage

### In DevContainer

The devcontainer is configured to automatically copy these secrets. If you need to manually load:

```fish
# Load all environment variables
export (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#')

# Or for fish shell
set -gx (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#' | string split '=')
```

### In Build Scripts

Scripts automatically load from environment or use defaults:

```javascript
const FEED_API_SECRET = process.env.FEED_API_SECRET || '...';
const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
```

## Active Resources

### Channel
- **ID**: `23b63744-649f-4274-add5-d1b439984e51`
- **URL**: https://feed.aesthetic.computer/api/v1/channels/23b63744-649f-4274-add5-d1b439984e51
- **Title**: KidLisp
- **Curator**: prompt.ac

### Current Playlists (as of October 6, 2025)
- **f60493a2-9e69-4e6b-837e-76047f48438c** - Top 100 as of Monday, October 6, 2025
- **2680b102-04ee-47b5-b7d7-f814094695e7** - Colors
- **e1bf1aae-2427-4dd0-a39d-f5da89fdf02e** - Chords for `clock`

## Security Notes

- Never commit `.env` or key files to public repositories
- API secrets provide full write access to the feed system
- MongoDB credentials have read access to aesthetic database
- Rotate keys periodically for security

## Deployment Keys

When deploying the Cloudflare Worker, these secrets must be set:

```bash
cd /workspaces/aesthetic-computer/feed/dp1-feed
wrangler secret put API_SECRET      # Use FEED_API_SECRET value
wrangler secret put ED25519_PRIVATE_KEY  # If using signed playlists
```

## Backup

These credentials are also stored in:
- 1Password (if applicable)
- Cloudflare Dashboard (for worker secrets)
- MongoDB Atlas (for database access)
