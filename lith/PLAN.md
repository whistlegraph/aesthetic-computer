# Netlify → DigitalOcean Migration Plan
# 2026-03-28

## Overview

Migrate aesthetic.computer from Netlify to a new DigitalOcean droplet.
Session server stays on its own droplet (157.245.134.225).

## Existing DO Droplets

| Droplet                  | IP              | Purpose                    |
|--------------------------|-----------------|----------------------------|
| session-server           | 157.245.134.225 | WebSocket + UDP sessions   |
| silo                     | 64.23.151.169   | MongoDB, feed              |
| oven                     | 137.184.237.166 | Image/code processing      |
| help                     | 146.190.150.173 | Help service               |
| judge                    | 64.227.102.108  | AI moderation              |
| pds                      | 165.227.120.137 | AT Protocol PDS (NYC3)     |
| **NEW: ac-web**          | **TBD**         | **Frontend + API server**  |

## Phase 1: Provision Droplet

```bash
doctl compute droplet create ac-web \
  --region sfo3 \
  --size s-2vcpu-4gb \
  --image ubuntu-24-04-x64 \
  --ssh-keys <your-key-id> \
  --tag-names ac,web
```

Why s-2vcpu-4gb ($24/mo):
- 125 Node.js API functions (some do Sharp/image processing)
- Static file serving
- Plenty of headroom vs Netlify's function limits

## Phase 2: Install Stack on Droplet

```bash
# Caddy (automatic HTTPS, reverse proxy)
apt install -y caddy

# Node.js 22 LTS
curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
apt install -y nodejs

# Sharp dependencies
apt install -y libvips-dev

# Chromium for puppeteer (screenshot function)
apt install -y chromium-browser

# PM2 for process management
npm install -g pm2
```

## Phase 3: Deploy Code

```bash
# Clone repo
git clone https://github.com/user/aesthetic-computer.git /opt/ac

# Install deps
cd /opt/ac/system && npm install

# Copy env vars
cp vault/netlify-production.env /opt/ac/system/.env
# Edit .env: remove NETLIFY_DEV, add NODE_ENV=production

# Start API server
pm2 start /opt/ac/system/server.mjs --name ac-api

# Or use systemd unit (see below)
```

## Phase 4: Caddy Config

```
# /etc/caddy/Caddyfile

# Main site — handles all Netlify subdomain routing
aesthetic.computer, www.aesthetic.computer,
api.aesthetic.computer,
bills.aesthetic.computer, give.aesthetic.computer,
keeps.aesthetic.computer, news.aesthetic.computer,
papers.aesthetic.computer, pals.aesthetic.computer,
l5.aesthetic.computer, p5.aesthetic.computer,
processing.aesthetic.computer, sitemap.aesthetic.computer,
kidlisp.com, www.kidlisp.com,
buy.kidlisp.com, calm.kidlisp.com, device.kidlisp.com,
keep.kidlisp.com, keeps.kidlisp.com, learn.kidlisp.com,
pj.kidlisp.com, top.kidlisp.com,
notepat.com, www.notepat.com,
prompt.ac, api.prompt.ac, l5.prompt.ac, p5.prompt.ac,
papers.prompt.ac, processing.prompt.ac, sitemap.prompt.ac,
sotce.net, www.sotce.net,
justanothersystem.org, www.justanothersystem.org,
builds.false.work {

    # Static files
    root * /opt/ac/system/public
    file_server

    # API functions → Node.js
    handle /api/* {
        reverse_proxy localhost:3000
    }
    handle /.netlify/functions/* {
        reverse_proxy localhost:3000
    }

    # Media proxy → DO Spaces
    handle /media/* {
        reverse_proxy localhost:3000
    }

    # Assets → DO Spaces CDN
    handle /assets/* {
        redir https://assets.aesthetic.computer{uri} 302
    }

    # SPA fallback (index.mjs function)
    handle {
        try_files {path} {path}/ /api/index
    }
}
```

Note: With Cloudflare proxied (orange cloud), Caddy won't need to handle
TLS itself. Use `http://` or set Cloudflare SSL to "Full" mode and let
Caddy use its auto-HTTPS with Cloudflare's origin certificate.

## Phase 5: Express Adapter (server.mjs)

Create a thin Express server that wraps existing Netlify function handlers:

```javascript
// system/server.mjs
import express from 'express';
import { readdirSync } from 'fs';

const app = express();
app.use(express.json());
app.use(express.raw({ type: '*/*', limit: '50mb' }));

// Load all functions
const fnDir = new URL('./netlify/functions/', import.meta.url);
const functions = {};

for (const file of readdirSync(fnDir)) {
  if (file.endsWith('.mjs') || file.endsWith('.js')) {
    const name = file.replace(/\.(mjs|js)$/, '');
    functions[name] = await import(`./netlify/functions/${file}`);
  }
}

// Netlify event adapter
function toNetlifyEvent(req) {
  return {
    httpMethod: req.method,
    headers: req.headers,
    body: typeof req.body === 'string' ? req.body : JSON.stringify(req.body),
    rawBody: req.body,
    queryStringParameters: req.query,
    path: req.path,
    isBase64Encoded: false,
  };
}

// Route: /api/:function or /.netlify/functions/:function
app.all(['/api/:fn', '/.netlify/functions/:fn'], async (req, res) => {
  const fn = functions[req.params.fn];
  if (!fn?.handler) return res.status(404).send('Function not found');

  try {
    const event = toNetlifyEvent(req);
    const context = { clientContext: {} };
    const result = await fn.handler(event, context);

    // Handle streaming (ask.js SSE)
    if (result.body && typeof result.body === 'object' && result.body.pipe) {
      result.body.pipe(res);
      return;
    }

    res.status(result.statusCode || 200);
    if (result.headers) res.set(result.headers);
    if (result.isBase64Encoded) {
      res.send(Buffer.from(result.body, 'base64'));
    } else {
      res.send(result.body);
    }
  } catch (err) {
    console.error(`Function ${req.params.fn} error:`, err);
    res.status(500).send('Internal Server Error');
  }
});

app.listen(3000, () => console.log('AC API on :3000'));
```

## Phase 6: DNS Cutover

Update all Netlify-pointing records to the new droplet IP.
See vault/cloudflare-dns-records.md for the complete list.

### Script to update all at once:

```bash
#!/bin/bash
NEW_IP="<DROPLET_IP>"
CF_EMAIL="me@jas.life"
CF_KEY="<cloudflare-api-key>"

# aesthetic.computer zone
ZONE="da794a6ae8f17b80424907f81ed0db7c"

# Update root A record
curl -X PATCH "https://api.cloudflare.com/client/v4/zones/$ZONE/dns_records/<record-id>" \
  -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY" \
  -H "Content-Type: application/json" \
  --data "{\"content\":\"$NEW_IP\"}"

# Update CNAMEs to A records pointing at new IP
# api.aesthetic.computer, bills, give, keeps, news, etc.
# Each CNAME → aesthetic-computer.netlify.app becomes A → $NEW_IP

# Repeat for: kidlisp.com, notepat.com, prompt.ac, sotce.net, etc.
```

### Records to Update (35 total):

**aesthetic.computer** (13 records):
- A aesthetic.computer → NEW_IP
- CNAME→A api, bills, give, keeps, l5, news, p5, pals, papers, processing, sitemap, www

**false.work** (1 record):
- CNAME→A builds.false.work

**justanothersystem.org** (2 records):
- A justanothersystem.org → NEW_IP
- CNAME→A www.justanothersystem.org

**kidlisp.com** (10 records):
- A kidlisp.com, www.kidlisp.com → NEW_IP
- CNAME→A buy, calm, device, keep, keeps, learn, pj, top

**notepat.com** (2 records):
- A notepat.com → NEW_IP
- CNAME→A www.notepat.com

**prompt.ac** (7 records):
- A prompt.ac → NEW_IP
- CNAME→A api, l5, p5, papers, processing, sitemap

**sotce.net** (2 records):
- A sotce.net, www.sotce.net → NEW_IP

**jas.life** (1 record — also points to Netlify):
- A jas.life → NEW_IP (currently 75.2.60.5)

**Total: 36 records across 8 zones**

## Phase 7: Stripe Webhook URLs

Update webhook endpoints in Stripe dashboard:
- aesthetic.computer Stripe account: `https://aesthetic.computer/api/print` (and /api/mug, /api/ticket, /api/give)
- sotce Stripe account: `https://sotce.net/api/ticket`

These should work as-is if DNS resolves to the new server correctly.

## Phase 8: Verify & Monitor

1. Test each API endpoint
2. Test Stripe webhooks (use Stripe CLI to send test events)
3. Test social previews (keeps-social edge function → Express middleware)
4. Test media proxy (media edge function → Express route)
5. Monitor PM2 logs: `pm2 logs ac-api`
6. Check Cloudflare analytics for error rates

## Rollback

If anything breaks, repoint DNS back to aesthetic-computer.netlify.app.
Netlify site stays intact as long as the account exists (even unpaid,
the site config persists — you just can't deploy).

## Cost Comparison

| Item          | Netlify    | DO Droplet  |
|---------------|------------|-------------|
| Hosting       | $19+/mo    | $24/mo      |
| Functions     | included   | included    |
| Bandwidth     | 100GB free | 4TB free    |
| SSL           | included   | Cloudflare  |
| CDN edge      | included   | Cloudflare  |
| Build minutes | 300/mo     | instant     |

Net: similar base cost but **much** more bandwidth and no function invocation limits.
