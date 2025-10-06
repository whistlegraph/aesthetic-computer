# Feed Worker Deployment

The Cloudflare Worker code for the DP-1 Feed API is located in:

```
/workspaces/aesthetic-computer/feed/dp1-feed/
```

This is the cloned repository from https://github.com/feral-file/dp1-feed

## Deployment

Deploy the worker to Cloudflare:

```bash
cd /workspaces/aesthetic-computer/feed/dp1-feed
wrangler deploy
```

This will deploy to: `feed.aesthetic.computer`

## Configuration

Worker configuration is in `wrangler.toml`:
- Account ID
- Route: `feed.aesthetic.computer/*`
- KV Namespace bindings
- Queue bindings
- Environment variables

## Secrets

Worker secrets must be set via wrangler CLI:

```bash
cd /workspaces/aesthetic-computer/feed/dp1-feed

# API Secret for authentication
wrangler secret put API_SECRET
# Enter: 008f7c7ceab429051d18370f5d580fcee453cdf0768c900d71660367feb95436

# ED25519 Private Key (if using signed playlists)
wrangler secret put ED25519_PRIVATE_KEY
```

## Worker Resources

The worker uses these Cloudflare resources:

### KV Namespaces
- `DP1_PLAYLISTS` - Stores playlist data
- `DP1_CHANNELS` - Stores channel metadata
- `DP1_PLAYLIST_ITEMS` - Stores individual items

### Queue
- `dp1-write-operations-prod` - Async write operations

### Environment Variables
Set in wrangler.toml or as secrets.

## Local Development

Run worker locally:

```bash
cd /workspaces/aesthetic-computer/feed/dp1-feed
wrangler dev
```

This starts a local server at http://localhost:8787

## Updating the Worker

1. Make changes to TypeScript source in `feed/dp1-feed/src/`
2. Test locally with `wrangler dev`
3. Deploy with `wrangler deploy`
4. Monitor logs: `wrangler tail`

## Architecture

The worker is built with:
- **TypeScript** - Type-safe JavaScript
- **Hono** - Fast web framework for Cloudflare Workers
- **Zod** - Schema validation
- **DP-1 Spec** - Distributed Playlist specification

## API Routes

- `GET /api/v1/channels` - List channels
- `GET /api/v1/channels/:id` - Get channel
- `POST /api/v1/channels` - Create channel
- `PATCH /api/v1/channels/:id` - Update channel
- `PUT /api/v1/channels/:id` - Replace channel
- `GET /api/v1/playlists` - List playlists
- `GET /api/v1/playlists/:id` - Get playlist
- `POST /api/v1/playlists` - Create playlist
- `DELETE /api/v1/playlists/:id` - Delete playlist

## Monitoring

View worker logs:

```bash
cd /workspaces/aesthetic-computer/feed/dp1-feed
wrangler tail
```

Or check Cloudflare Dashboard:
https://dash.cloudflare.com → Workers & Pages → feed.aesthetic.computer
