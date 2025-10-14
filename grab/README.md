# Grab Worker

Screenshot generation service for Aesthetic Computer pieces using Cloudflare Workers and Browser Rendering API.

## 🚀 Quick Start

**Production URL**: https://grab.aesthetic.computer

### API Endpoints

#### Icon (Small)
```bash
GET /icon/{width}x{height}/{piece}.png
```
Example: `https://grab.aesthetic.computer/icon/128x128/prompt.png`

#### Preview (Large)
```bash
GET /preview/{width}x{height}/{piece}.png
```
Example: `https://grab.aesthetic.computer/preview/1200x630/prompt.png`

#### Health Check
```bash
GET /health
```
Example: `https://grab.aesthetic.computer/health`

### Common Sizes

| Size | Use Case | Example |
|------|----------|---------|
| `128x128` | Favicon, small icons | `/icon/128x128/prompt.png` |
| `256x256` | App icons | `/icon/256x256/prompt.png` |
| `512x512` | Large icons | `/icon/512x512/prompt.png` |
| `1200x630` | Open Graph images | `/preview/1200x630/prompt.png` |
| `1800x900` | Twitter cards | `/preview/1800x900/prompt.png` |

### Query Parameters

Pass parameters to the piece:
```bash
/icon/128x128/prompt.png?text=hello&color=red
```

---

## 📦 Features

- ✅ **Fast Screenshots**: 7-15 seconds for first request, <1s cached
- ✅ **Durable Sessions**: Browser instances reused for efficiency
- ✅ **Smart Caching**: 1hr browser cache, 24hr CDN cache
- ✅ **Custom Domains**: Clean URLs via Cloudflare Workers
- ✅ **Rate Limiting**: Built-in protection
- ✅ **Error Handling**: Graceful fallbacks and timeouts

---

## 🛠️ Development

### Install Dependencies
```bash
npm install
```

### Type Check
```bash
npm run type-check
```

### Deploy
```bash
npm run deploy
```

### View Logs
```bash
npm run logs
```

### Local Development
```bash
npm run dev
```

Note: Local development uses `wrangler dev` which connects to remote Browser Rendering API.

---

## 📚 Documentation

- **[DEPLOY.md](DEPLOY.md)**: Complete deployment guide
- **[VIDEO-FUTURE.md](VIDEO-FUTURE.md)**: Future video capture infrastructure
- **[docs/](docs/)**: Detailed technical documentation
  - `ANIMATED-GIF-STATUS.md`: Video recording research
  - `INTEGRATION-SUMMARY.md`: System integration details
  - `DEV-SETUP.md`: Local development guide
  - `DEPLOYMENT.md`: Legacy deployment docs
  - `CODE-CHANGES.md`: Change history

---

## 🏗️ Architecture

```
User → grab.aesthetic.computer
       ↓
       Cloudflare Workers (aesthetic-grab)
       ↓
       Durable Object (Browser session)
       ↓
       Browser Rendering API (@cloudflare/puppeteer)
       ↓
       Screenshot (PNG) + Cache Headers
       ↓
       Cloudflare CDN (24hr cache)
```

### Stack
- **Cloudflare Workers**: Serverless compute
- **Browser Rendering API**: Headless Chrome for screenshots
- **Durable Objects**: Browser session management
- **TypeScript**: Type-safe code
- **@cloudflare/puppeteer**: Browser automation

---

## ⚙️ Configuration

Environment variables in `wrangler.toml`:

```toml
[vars]
ENVIRONMENT = "production"
DOMAIN = "grab.aesthetic.computer"
CACHE_TTL_SECONDS = "3600"           # 1 hour
CDN_CACHE_TTL_SECONDS = "86400"      # 24 hours
MAX_SCREENSHOT_AGE_MS = "604800000"  # 7 days
BROWSER_TIMEOUT_MS = "30000"         # 30 seconds
MAX_VIEWPORT_WIDTH = "1920"
MAX_VIEWPORT_HEIGHT = "1080"
MAX_REQUESTS_PER_MINUTE = "60"
MAX_BROWSER_SESSIONS = "10"
```

---

## 🧪 Testing

### Health Check
```bash
curl "https://grab.aesthetic.computer/health"
```

### Generate Icon
```bash
curl "https://grab.aesthetic.computer/icon/128x128/prompt.png" --output test.png
file test.png
# Expected: PNG image data, 128 x 128
```

### Generate Preview
```bash
curl "https://grab.aesthetic.computer/preview/1200x630/prompt.png" --output test.png
file test.png
# Expected: PNG image data, 1200 x 630
```

### Check Cache
```bash
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
# Look for: CF-Cache-Status: HIT (on second request)
```

---

## 🔗 Integration

This worker replaces the Netlify Edge Function for screenshot generation.

### parse.mjs
```javascript
const iconURL = `https://grab.aesthetic.computer/icon/128x128/${pieceParam}${query}`;
const previewURL = `https://grab.aesthetic.computer/preview/1200x630/${pieceParam}${query}`;
```

### netlify.toml
```toml
# Icon redirect
[[redirects]]
  from = "/aesthetic.computer/api/icon/:piece"
  to = "https://grab.aesthetic.computer/icon/128x128/:piece.png"
  status = 200

# Preview redirect  
[[redirects]]
  from = "/aesthetic.computer/api/preview/:piece"
  to = "https://grab.aesthetic.computer/preview/1200x630/:piece.png"
  status = 200
```

---

## 📊 Monitoring

### Cloudflare Dashboard
https://dash.cloudflare.com → Workers & Pages → aesthetic-grab

### Key Metrics
- **Success Rate**: >99%
- **Response Time**: 7-15s (first), <1s (cached)
- **Cache Hit Rate**: Increases over time
- **Error Rate**: Monitor 4xx/5xx

### View Logs
```bash
npm run logs
```

### Recent Deployments
```bash
npx wrangler deployments list
```

---

## 🐛 Troubleshooting

### Screenshots are blank/white
- Piece may not exist on aesthetic.computer
- Check logs: `npm run logs`
- Verify Browser Rendering API is active

### Slow response times
- First request always slower (browser cold start)
- Check if Durable Object session reuse is working
- Adjust `BROWSER_TIMEOUT_MS` if needed

### DNS issues
- Wait 5-10 minutes for propagation
- Check: `dig grab.aesthetic.computer`
- Verify `routes` in wrangler.toml

### Deployment failures
- Ensure Workers Paid plan is active
- Check Browser Rendering API is enabled
- Verify account_id in wrangler.toml

---

## 🔮 Future: Video Capture

Video/animation capture requires different infrastructure than Cloudflare Workers. See **[VIDEO-FUTURE.md](VIDEO-FUTURE.md)** for the roadmap:

- Digital Ocean VPS with GPU support
- Real-time video capture (30-60 FPS)
- FFmpeg with hardware encoding
- Similar to `/at` PDS deployment model

---

## 📝 License

Part of the Aesthetic Computer project.

**Last Updated**: October 14, 2025  
**Version**: e77346d0-3732-4bd2-b03b-e932dc50fac9
