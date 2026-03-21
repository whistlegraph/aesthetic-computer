# Grab Worker - Development Setup

## Overview

The grab worker handles screenshot generation for aesthetic.computer. This guide covers local development and testing.

---

## Local Development

### Option 1: Wrangler Dev (Recommended)

Run the worker locally with Cloudflare's dev environment:

```fish
cd /workspaces/aesthetic-computer/grab
npm run dev
```

This starts a local server at `http://127.0.0.1:8787`

**Test locally:**
```fish
curl -I "http://127.0.0.1:8787/icon/128x128/prompt.png"
```

### Option 2: Remote Testing

Use the deployed worker during development:

```fish
# Production worker
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"

# Or workers.dev URL
curl -I "https://aesthetic-grab.aesthetic-computer.workers.dev/icon/128x128/prompt.png"
```

---

## Integration with Netlify Dev

When running `netlify dev` (typically on `http://localhost:8888`), the redirects in `netlify.toml` will proxy requests to the grab worker:

1. **Local aesthetic.computer** makes request to `/icon/128x128/prompt.png`
2. **Netlify redirect** proxies to `https://grab.aesthetic.computer/icon/128x128/prompt.png`
3. **Grab worker** generates and returns screenshot

### Testing the Full Stack

```fish
# Start netlify dev
cd /workspaces/aesthetic-computer
netlify dev

# In another terminal, test icon generation
curl -I "http://localhost:8888/icon/128x128/prompt.png"

# Should see redirect to grab.aesthetic.computer
```

---

## Dev Stack Integration

### Adding to Emacs Web Panels

To run the grab worker in your dev stack alongside other services:

1. **Create an Emacs pane** for the grab worker:
   ```fish
   # In your dev startup script or manually
   cd /workspaces/aesthetic-computer/grab && npm run dev
   ```

2. **Monitor logs** with wrangler tail:
   ```fish
   cd /workspaces/aesthetic-computer/grab
   npx wrangler tail
   ```

3. **Check worker status:**
   ```fish
   cd /workspaces/aesthetic-computer/grab
   npx wrangler dev --test-scheduled
   ```

### Environment Variables for Development

The worker automatically detects localhost URLs and adjusts timeouts:

- Production timeout: 30 seconds
- Development: Can increase if needed by updating `wrangler.toml`

---

## Debugging

### View Real-time Logs

```fish
cd /workspaces/aesthetic-computer/grab
npx wrangler tail

# In another terminal, trigger a screenshot
curl "https://grab.aesthetic.computer/icon/256x256/prompt.png" > /dev/null
```

### Common Issues

**1. Empty Screenshots**
- Ensure `@cloudflare/puppeteer` is latest version (not 0.0.1)
- Check browser binding is active in wrangler.toml

**2. Timeout Errors**
- Increase `SCREENSHOT_TIMEOUT_MS` in wrangler.toml
- Check if aesthetic.computer piece loads properly

**3. 404 on Custom Domain**
- Custom domain may not be set up yet
- Use `aesthetic-grab.aesthetic-computer.workers.dev` URL instead

**4. Canvas Not Found**
- Piece may not have rendered yet
- Increase `RENDER_DELAY_MS` in wrangler.toml
- Check if piece uses `window.preloaded` flag

---

## Local vs Production URLs

### Production (After Custom Domain Setup)
```
https://grab.aesthetic.computer/icon/128x128/prompt.png
https://grab.aesthetic.computer/preview/1200x630/prompt.png
```

### Workers.dev (Always Available)
```
https://aesthetic-grab.aesthetic-computer.workers.dev/icon/128x128/prompt.png
https://aesthetic-grab.aesthetic-computer.workers.dev/preview/1200x630/prompt.png
```

### Local Development
```
http://127.0.0.1:8787/icon/128x128/prompt.png
http://127.0.0.1:8787/preview/1200x630/prompt.png
```

---

## Testing Query Parameters

The worker constructs URLs with query parameters to tell the aesthetic.computer client which resolution to render:

```fish
# Icon request becomes:
# https://aesthetic.computer/prompt?icon=128x128

# Preview request becomes:
# https://aesthetic.computer/prompt?preview=1200x630

# Test this by checking logs:
npx wrangler tail
```

You should see log messages like:
```
Taking screenshot: 128x128 of https://aesthetic.computer/prompt?icon=128x128
Canvas element found
Screenshot generated successfully
```

---

## Hot Reloading

Wrangler dev supports hot reloading:

1. Make changes to `src/index.ts`
2. Save the file
3. Wrangler automatically rebuilds and restarts
4. Test with curl immediately

---

## Performance Monitoring

### Check Screenshot Generation Time

```fish
curl -w "\nTime: %{time_total}s\n" -o /dev/null -s \
  "https://grab.aesthetic.computer/icon/128x128/prompt.png"
```

Typical times:
- **First request** (cold start): ~8-12 seconds
- **Cached requests**: < 1 second
- **Browser reuse**: ~3-5 seconds

### Monitor Durable Object State

Check if browser sessions are being reused:

```fish
npx wrangler tail --format json | jq '.logs[] | select(.message | contains("Browser DO"))'
```

You should see:
- `Browser DO: setting alarm` - Browser staying alive
- `Browser DO: alarm triggered` - Browser cleanup after 60s

---

## Next Steps

- [ ] Set up custom domain `grab.aesthetic.computer` via Cloudflare Dashboard
- [ ] Add grab worker startup to main dev scripts
- [ ] Configure Emacs web panel for grab worker logs
- [ ] Test integration with various pieces (wipe, line, etc.)
- [ ] Monitor error rates in production
