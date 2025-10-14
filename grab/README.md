# Grab Worker - Browser Rendering Service

Screenshot and preview generation service using Cloudflare Browser Rendering API.

## Files

- `.env` - Environment variables for grab worker deployment
- `wrangler.production.toml` - Cloudflare Workers production configuration

## Usage

### Loading Secrets in DevContainer

Secrets are automatically loaded via `/aesthetic-computer-vault/devault.fish` on container start.

### Manual Secret Loading

```fish
# Load environment variables (fish shell)
set -gx (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/grab/.env | grep -v '^#' | string split '=')
```

### Deploying Grab Worker

```fish
# Copy secrets to working directory
cd /workspaces/aesthetic-computer/aesthetic-computer-vault
./devault.fish

# Deploy
cd /workspaces/aesthetic-computer/grab
npm run deploy:production
```

### Setting Cloudflare Secrets

```fish
cd /workspaces/aesthetic-computer/grab

# If you have sensitive API keys (not currently needed)
wrangler secret put BROWSER_API_KEY
```

## Initial Setup

### 1. Get Cloudflare API Token (for DNS automation)

**Required for automatic DNS configuration during deployment.**

1. Go to: https://dash.cloudflare.com/profile/api-tokens
2. Click: **"Create Token"**
3. Use template: **"Edit zone DNS"**
4. Configure:
   - **Permissions:** Zone → DNS → Edit
   - **Zone Resources:** Include → Specific zone → aesthetic.computer
5. Click: **"Continue to summary"** → **"Create Token"**
6. Copy the token and add to `.env`:
   ```bash
   CLOUDFLARE_API_TOKEN=your-token-here
   ```

**Note:** This is different from `CLOUDFLARE_API_KEY` (Global API Key). The API Token has scoped permissions for DNS only.

### 2. Create Browser Rendering Binding

```fish
cd /workspaces/aesthetic-computer/grab
./scripts/setup-browser-binding.fish
```

This will:
- Enable Browser Rendering in your Cloudflare account
- Create the binding
- Output the binding ID
- Update the `.env` file with the ID

### 3. Create Durable Object Namespace

```fish
wrangler dispatch-namespace create SCREENSHOT_DO
```

Copy the namespace ID to `.env`:
```bash
DO_NAMESPACE_ID=<paste-id-here>
```

### 4. Create R2 Bucket (Optional)

```fish
wrangler r2 bucket create aesthetic-screenshots
```

Copy the bucket name/ID to `.env`.

### 5. Deploy with Automatic DNS

**One-command deployment with automatic DNS configuration:**

```fish
cd /workspaces/aesthetic-computer/grab
./scripts/deploy-with-dns.fish
```

This script will:
1. ✅ Deploy the worker to Cloudflare
2. ✅ Automatically create/update DNS CNAME record
3. ✅ Wait for DNS propagation
4. ✅ Verify the deployment is accessible

**Manual deployment (without DNS automation):**

```fish
cd /workspaces/aesthetic-computer/grab
npm run deploy:production

# Then manually configure DNS via Dashboard or use:
./scripts/configure-dns.fish
```

### 6. Alternative: Manual DNS Configuration

If the automated script doesn't work, you can manually configure DNS:

**Option A: Cloudflare Dashboard (2 minutes)**
1. Go to: https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records
2. Click: "Add record"
3. Type: CNAME
4. Name: `grab`
5. Target: `aesthetic-grab.aesthetic-computer.workers.dev`
6. Proxy: ✅ Enabled (orange cloud)
7. Save

**Option B: Workers Dashboard (5 minutes)**
1. Go to Workers & Pages
2. Click on `aesthetic-grab`
3. Go to Settings > Domains & Routes
4. Click "Add Custom Domain"
5. Enter: `grab.aesthetic.computer`
6. Click "Add Domain"

Either method works - automated deployment script is fastest!

## Architecture

```
Client Request (grab.aesthetic.computer)
    ↓
Cloudflare Worker
    ↓
Check Cache API (ETag)
    ↓ (miss)
Durable Object (per URL)
    ↓
Browser Rendering API
    ↓
Puppeteer Screenshot
    ↓
Store in Cache + R2
    ↓
Return PNG
```

## Endpoints

### Icon Generation
```
GET https://grab.aesthetic.computer/icon/128x128/prompt~wipe.png
GET https://grab.aesthetic.computer/icon/128x128/welcome.png
```

### Preview Generation (OG Image)
```
GET https://grab.aesthetic.computer/preview/1200x630/prompt~wipe.png
```

### Preview Generation (Twitter Card)
```
GET https://grab.aesthetic.computer/preview/1800x900/prompt~wipe.png
```

## Monitoring

### View Logs
```fish
# Real-time logs
wrangler tail aesthetic-grab

# Production logs
wrangler tail aesthetic-grab --env production
```

### Metrics

View in Cloudflare Dashboard:
- Workers & Pages > aesthetic-grab > Metrics
- Analytics > Workers
- Durable Objects > SCREENSHOT_DO

## Troubleshooting

### Browser Rendering Not Working

1. Check browser binding is enabled:
```fish
wrangler whoami
# Ensure Browser Rendering is in enabled features
```

2. Check DO namespace exists:
```fish
wrangler dispatch-namespace list
```

3. Check logs:
```fish
wrangler tail aesthetic-grab
```

### Caching Issues

Clear CDN cache:
```fish
# Via Cloudflare Dashboard:
# Caching > Configuration > Purge Cache
# Or use Cloudflare API
```

### Performance Issues

- Check browser timeout settings
- Monitor DO session count
- Check R2 storage limits
- Review cache hit rates

## Security

- All secrets stored in vault (not in repo)
- API keys managed via wrangler secrets
- Rate limiting per IP
- URL validation (aesthetic.computer only)
- Resource limits enforced

## Cost Optimization

### Browser Rendering
- Charged per second of browser time
- Optimize page load times
- Cache aggressively
- Set reasonable timeouts

### Durable Objects
- Charged per request and duration
- Minimize DO calls via caching
- Batch operations when possible

### R2 Storage
- Class A operations (write)
- Class B operations (read)
- Storage capacity
- Monitor usage via dashboard

## Related Documentation

- Main implementation: `/workspaces/aesthetic-computer/grab/`
- Deployment plan: `/workspaces/aesthetic-computer/grab/PLAN.md`
- Migration guide: `/workspaces/aesthetic-computer/grab/DEPLOYMENT.md`
