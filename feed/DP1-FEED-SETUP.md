# DP1 Feed Worker - Complete Setup Guide

## 📋 Overview

I've integrated the DP1 Feed Operator API into your aesthetic-computer repository. The worker will be deployed to **feed.aesthetic.computer** as a Cloudflare Worker.

### What's Been Done

1. ✅ **Cloned Repository**: Added `dp1-feed/` to your repo
2. ✅ **Updated Configuration**: Modified `wrangler.toml` for aesthetic.computer domain
3. ✅ **Created Scripts**: Setup and deployment automation scripts
4. ✅ **Added Package Scripts**: Convenience commands in main package.json
5. ✅ **Added ripgrep**: Installed `rg` in your Docker container and Dockerfile
6. ✅ **Found Cloudflare Credentials**: Located in `dark-window/conductor.env`

### Your Cloudflare Credentials

Found in `/workspaces/aesthetic-computer/dark-window/conductor.env`:
- **Email**: me@jas.life
- **API Token**: YOUR_CLOUDFLARE_API_KEY_HERE
- **Account ID**: a23b54e8877a833a1cf8db7765bce3ca (from your DNS link)

## 🚀 Deployment Steps

### Step 1: Authenticate Wrangler

```fish
# Set environment variables
set -x CLOUDFLARE_API_TOKEN "YOUR_CLOUDFLARE_API_KEY_HERE"
set -x CLOUDFLARE_EMAIL "me@jas.life"

# Authenticate wrangler (choose one method)
# Option A: Interactive login (opens browser)
cd dp1-feed
wrangler login

# Option B: Use API token directly
wrangler config
# When prompted, paste your API token
```

### Step 2: Create Cloudflare Resources

Run the setup script to create KV namespaces and queues:

```fish
cd dp1-feed
./setup-resources.fish
```

Or manually from the root:
```fish
npm run feed:setup
```

**Important**: After running this, you'll see output with KV namespace IDs. You need to:
1. Copy those IDs
2. Update `dp1-feed/wrangler.toml` in the `[env.production.kv_namespaces]` sections

Example output:
```
Created namespace with title "aesthetic-feed-DP1_PLAYLISTS"
 ID: abc123def456...

Created namespace with title "aesthetic-feed-DP1_CHANNELS"
 ID: ghi789jkl012...
```

Update wrangler.toml:
```toml
[[env.production.kv_namespaces]]
binding = "DP1_PLAYLISTS"
id = "abc123def456..."  # <-- Update this

[[env.production.kv_namespaces]]
binding = "DP1_CHANNELS"
id = "ghi789jkl012..."  # <-- Update this
```

### Step 3: Set Secrets

Generate and set required secrets:

```fish
cd dp1-feed

# Generate a secure API secret
set -l api_secret (openssl rand -hex 32)
echo "Generated API Secret: $api_secret"
echo $api_secret | wrangler secret put API_SECRET --env production

# Generate Ed25519 key pair for signing
npm run jwt:generate-keys
# This creates private-key.txt and public-key.txt

# Set the private key as secret
cat private-key.txt | wrangler secret put ED25519_PRIVATE_KEY --env production

# Optional: Set JWT configuration if using JWT auth
echo "your-issuer" | wrangler secret put JWT_ISSUER --env production
echo "your-audience" | wrangler secret put JWT_AUDIENCE --env production
```

**Save your API secret!** You'll need it for write operations.

### Step 4: Deploy the Worker

```fish
# From root
npm run feed:deploy

# Or from dp1-feed directory
cd dp1-feed
./deploy-feed.fish production
```

### Step 5: Set Up Custom Domain

After deployment, you need to configure the custom domain:

#### Option A: Cloudflare Dashboard (Recommended)

1. Go to https://dash.cloudflare.com/
2. Select your account
3. Go to **Workers & Pages**
4. Click on **aesthetic-feed**
5. Go to **Settings** > **Domains & Routes**
6. Click **Add Custom Domain**
7. Enter: `feed.aesthetic.computer`
8. Click **Add Domain**

Cloudflare will automatically:
- Create the necessary DNS CNAME record
- Set up SSL/TLS
- Route traffic to your worker

#### Option B: Using Cloudflare API (Automated)

You can automate this using your existing `conductor.mjs` pattern. Create a script:

```javascript
// deploy-feed-domain.mjs
import fetch from 'node-fetch';

const CLOUDFLARE_EMAIL = process.env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_TOKEN = process.env.CLOUDFLARE_API_TOKEN;
const ZONE_ID = 'a23b54e8877a833a1cf8db7765bce3ca';
const WORKER_NAME = 'aesthetic-feed';

const headers = {
  'X-Auth-Email': CLOUDFLARE_EMAIL,
  'X-Auth-Key': CLOUDFLARE_API_TOKEN,
  'Content-Type': 'application/json',
};

// Add custom domain to worker
const response = await fetch(
  `https://api.cloudflare.com/client/v4/accounts/${ZONE_ID}/workers/domains`,
  {
    method: 'POST',
    headers,
    body: JSON.stringify({
      hostname: 'feed.aesthetic.computer',
      service: WORKER_NAME,
      environment: 'production'
    })
  }
);

const result = await response.json();
console.log(result);
```

### Step 6: Test the Deployment

```fish
# Health check
curl https://feed.aesthetic.computer/api/v1/health

# Get API info
curl https://feed.aesthetic.computer/api/v1

# Test creating a playlist (use your API_SECRET)
curl -X POST https://feed.aesthetic.computer/api/v1/playlists \
  -H "Authorization: Bearer YOUR_API_SECRET_HERE" \
  -H "Content-Type: application/json" \
  -d '{
    "dpVersion": "1.0.0",
    "title": "aesthetic-test",
    "items": [
      {
        "source": "https://aesthetic.computer/prompt",
        "duration": 300,
        "license": "open"
      }
    ]
  }'
```

## 📝 Available Commands

From the root of aesthetic-computer:

```fish
npm run feed:dev           # Run local dev server
npm run feed:setup         # Create Cloudflare resources
npm run feed:deploy        # Deploy to production
npm run feed:deploy:dev    # Deploy to dev environment
npm run feed:test          # Run API tests
npm run feed:logs          # Watch production logs
```

## 🔍 Monitoring & Debugging

### View Real-time Logs

```fish
cd dp1-feed
wrangler tail --env production
```

Or from root:
```fish
npm run feed:logs
```

### Check Worker Status

Visit: https://dash.cloudflare.com/
- Navigate to Workers & Pages > aesthetic-feed
- View metrics, logs, and errors

### Local Development

Run the worker locally:
```fish
cd dp1-feed
npm run worker:dev
# Visit http://localhost:8787/api/v1
```

## 📚 API Documentation

### Authentication

All write operations require authentication:
```
Authorization: Bearer YOUR_API_SECRET
```

### Key Endpoints

- `GET /api/v1` - API information
- `GET /api/v1/health` - Health check
- `GET /api/v1/playlists` - List all playlists
- `POST /api/v1/playlists` - Create a playlist (auth required)
- `GET /api/v1/playlists/{id}` - Get specific playlist
- `PUT /api/v1/playlists/{id}` - Update playlist (auth required)
- `DELETE /api/v1/playlists/{id}` - Delete playlist (auth required)
- `GET /api/v1/channels` - List all channels
- `POST /api/v1/channels` - Create a channel (auth required)

### Async Operations

For high-throughput scenarios, use async mode:
```bash
curl -X POST https://feed.aesthetic.computer/api/v1/playlists \
  -H "Authorization: Bearer YOUR_API_SECRET" \
  -H "Prefer: respond-async" \
  -H "Content-Type: application/json" \
  -d '...'
# Returns 202 Accepted immediately
```

## 🎨 Integration with Aesthetic Computer

### Using the Feed API in Pieces

```javascript
// Example piece that fetches playlists
export async function boot({ api }) {
  const response = await fetch('https://feed.aesthetic.computer/api/v1/playlists');
  const playlists = await response.json();
  console.log('Playlists:', playlists);
}
```

### Creating Playlists from Pieces

```javascript
export async function boot({ api }) {
  // You would store API_SECRET securely, not in client code
  const playlist = {
    dpVersion: "1.0.0",
    title: "my-aesthetic-playlist",
    items: [
      {
        source: "https://aesthetic.computer/wand",
        duration: 300,
        license: "open"
      }
    ]
  };
  
  const response = await fetch('https://feed.aesthetic.computer/api/v1/playlists', {
    method: 'POST',
    headers: {
      'Authorization': 'Bearer YOUR_API_SECRET',
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(playlist)
  });
  
  const result = await response.json();
  console.log('Created playlist:', result);
}
```

## 🔧 Troubleshooting

### "Worker not found" error
- Wait a few minutes after deployment
- Check deployment status: `wrangler deployments list --env production`
- Verify worker name matches in wrangler.toml

### Custom domain not working
- Check DNS propagation: `dig feed.aesthetic.computer`
- Verify custom domain is added in Cloudflare dashboard
- Wait up to 5 minutes for DNS changes

### KV errors
- Verify namespace IDs in wrangler.toml match created namespaces
- Check bindings are correct (DP1_PLAYLISTS, DP1_CHANNELS, DP1_PLAYLIST_ITEMS)

### Queue errors
- Verify queue name matches in wrangler.toml
- Check queue exists: `wrangler queues list`

### Authentication errors
- Verify API_SECRET is set: `wrangler secret list --env production`
- Check Bearer token format in requests
- Ensure ED25519_PRIVATE_KEY is properly formatted

## 📖 Additional Resources

- **Original Repo**: https://github.com/feral-file/dp1-feed
- **DP-1 Spec**: https://github.com/display-protocol/dp1
- **Full API Docs**: See `dp1-feed/README.md` and `dp1-feed/DEVELOPMENT.md`
- **OpenAPI Schema**: See `dp1-feed/openapi.yaml`
- **Cloudflare Docs**: https://developers.cloudflare.com/workers/

## 🎯 Next Steps

1. Run `npm run feed:setup` to create resources
2. Update namespace IDs in wrangler.toml
3. Set secrets with `wrangler secret put`
4. Deploy with `npm run feed:deploy`
5. Set up custom domain in Cloudflare dashboard
6. Test with `curl https://feed.aesthetic.computer/api/v1/health`

---

**Need help?** Check the detailed setup guide in `dp1-feed/AESTHETIC-COMPUTER-SETUP.md`
