# Code Changes Summary - Grab Worker Integration

This document shows all code changes made to integrate the Cloudflare grab worker with the existing aesthetic.computer codebase.

---

## Changed Files

### 1. `/system/public/aesthetic.computer/lib/parse.mjs`

**Purpose:** Generate metadata (title, description, og:image, icon) for pieces

**Lines Changed:** 487-494

**Before:**
```javascript
  } else {
    ogImage = `https://${host}/preview/1200x630/${slug}.png`;
    twitterImage = `https://${host}/preview/1800x900/${slug}.png`;
  }

  // Extract just the piece name (before ~) for icon URL
  const pieceName = slug.split('~')[0];
  icon = pieceMetadata?.icon_url || `${protocol}//${host}/icon/128x128/${pieceName}.png`;
```

**After:**
```javascript
  } else {
    // Use Cloudflare Workers grab service for screenshot generation
    ogImage = `https://grab.aesthetic.computer/preview/1200x630/${slug}.png`;
    twitterImage = `https://grab.aesthetic.computer/preview/1800x900/${slug}.png`;
  }

  // Extract just the piece name (before ~) for icon URL
  const pieceName = slug.split('~')[0];
  icon = pieceMetadata?.icon_url || `https://grab.aesthetic.computer/icon/128x128/${pieceName}.png`;
```

**Impact:**
- All og:image meta tags now point to grab.aesthetic.computer
- All favicon/icon links now point to grab.aesthetic.computer
- Removes dependency on `${host}` variable (always uses grab.aesthetic.computer)
- Removes `${protocol}` variable (always uses HTTPS)

**Example Output:**
```html
<!-- Before -->
<meta name="og:image" content="https://aesthetic.computer/preview/1200x630/prompt.png" />
<link rel="icon" href="https://aesthetic.computer/icon/128x128/prompt.png" />

<!-- After -->
<meta name="og:image" content="https://grab.aesthetic.computer/preview/1200x630/prompt.png" />
<link rel="icon" href="https://grab.aesthetic.computer/icon/128x128/prompt.png" />
```

---

### 2. `/system/netlify.toml`

**Purpose:** Configure Netlify redirects and routing

**Section 1 - Lines 285-290 (Preview Redirect)**

**Before:**
```toml
[[redirects]]
from = "/preview/*"
to = "/.netlify/functions/screenshot"
status = 200
```

**After:**
```toml
# Screenshot generation handled by Cloudflare Workers (grab.aesthetic.computer)
[[redirects]]
from = "/preview/*"
to = "https://grab.aesthetic.computer/preview/:splat"
status = 200
```

**Section 2 - Lines 306-310 (Icon Redirect)**

**Before:**
```toml
[[redirects]]
from = "/icon/*"
to = "/.netlify/functions/screenshot"
status = 200
```

**After:**
```toml
[[redirects]]
from = "/icon/*"
to = "https://grab.aesthetic.computer/icon/:splat"
status = 200
```

**Impact:**
- Requests to `aesthetic.computer/preview/*` now proxy to `grab.aesthetic.computer/preview/*`
- Requests to `aesthetic.computer/icon/*` now proxy to `grab.aesthetic.computer/icon/*`
- No more calls to Netlify function `screenshot.js`
- Maintains backwards compatibility for existing URLs

**Example Behavior:**
```bash
# User requests:
https://aesthetic.computer/icon/128x128/prompt.png

# Netlify redirects to:
https://grab.aesthetic.computer/icon/128x128/prompt.png

# Grab worker returns:
PNG image data (399 bytes)
```

---

## New Files Created

### `/grab/` Directory Structure
```
grab/
├── src/
│   └── index.ts              # Main worker implementation
├── scripts/
│   └── deploy.fish           # Deployment automation
├── package.json              # Dependencies
├── wrangler.toml             # Worker configuration
├── tsconfig.json             # TypeScript config
├── PLAN.md                   # Project roadmap
├── README.md                 # Architecture docs
├── DEPLOYMENT.md             # Deployment guide
├── MIGRATION.md              # Migration strategy
├── DEV-SETUP.md              # Dev workflow
├── INTEGRATION-SUMMARY.md    # This integration summary
├── CUSTOM-DOMAIN-SETUP.md    # Custom domain setup guide
└── CODE-CHANGES.md           # Detailed code changes (this file)
```

### `/aesthetic-computer-vault/grab/` Directory
```
aesthetic-computer-vault/grab/
├── .env                      # Environment variables
├── README.md                 # Vault documentation
└── wrangler.production.toml  # Production config
```

---

## Files NOT Changed

### Static Templates
- `/system/templates/index.html` - Only has fallback og:image, dynamic content uses parse.mjs
- No changes needed

### Other Netlify Functions
- `/system/netlify/functions/index.mjs` - No changes needed (imports metadata from parse.mjs)
- `/system/netlify/functions/docs.js` - References icon but doesn't need updating
- All other functions unchanged

### Client-Side Code
- `/system/public/aesthetic.computer/boot.js` - No changes needed
- Pieces remain unchanged
- No JavaScript modifications required

---

## Validation Commands

### Test Parse.mjs Changes
```fish
# Check that metadata function returns grab URLs
cd /workspaces/aesthetic-computer
grep -A 5 "ogImage =" system/public/aesthetic.computer/lib/parse.mjs

# Expected output should show:
# ogImage = `https://grab.aesthetic.computer/preview/1200x630/${slug}.png`;
```

### Test netlify.toml Changes
```fish
# Check redirect configuration
grep -A 3 "from = \"/preview/" system/netlify.toml

# Expected output should show:
# to = "https://grab.aesthetic.computer/preview/:splat"
```

### Test End-to-End
```fish
# After custom domain is set up:
curl -I "https://aesthetic.computer/icon/128x128/prompt.png"

# Should return:
# HTTP/2 200
# location: https://grab.aesthetic.computer/icon/128x128/prompt.png
# (or directly serve the image)
```

---

## Rollback Instructions

If you need to revert these changes:

### 1. Revert parse.mjs
```fish
cd /workspaces/aesthetic-computer
git checkout system/public/aesthetic.computer/lib/parse.mjs
```

Or manually change back to:
```javascript
ogImage = `https://${host}/preview/1200x630/${slug}.png`;
icon = pieceMetadata?.icon_url || `${protocol}//${host}/icon/128x128/${pieceName}.png`;
```

### 2. Revert netlify.toml
```fish
git checkout system/netlify.toml
```

Or manually change back to:
```toml
[[redirects]]
from = "/preview/*"
to = "/.netlify/functions/screenshot"
status = 200

[[redirects]]
from = "/icon/*"
to = "/.netlify/functions/screenshot"
status = 200
```

### 3. Keep Old Implementation Running
- DO NOT delete `/system/netlify/functions/screenshot.js` yet
- Leave Puppeteer dependencies in package.json
- Monitor for any issues

---

## Deployment Strategy

### Phase 1: Deploy Worker ✅
- Worker deployed and tested
- URLs accessible via workers.dev

### Phase 2: Configure Custom Domain (Pending)
- Set up grab.aesthetic.computer via Dashboard
- DNS propagation
- SSL certificate provisioning

### Phase 3: Update Code ✅
- parse.mjs updated
- netlify.toml updated
- Both point to grab.aesthetic.computer

### Phase 4: Validation (Next)
- Test production site
- Monitor for errors
- Verify social media previews
- Check various pieces

### Phase 5: Cleanup (After 1 week)
- Remove screenshot.js
- Remove Puppeteer dependencies
- Update documentation

---

## Testing Checklist

After deploying these changes:

- [ ] **Metadata Generation**
  - [ ] Icon URLs use grab.aesthetic.computer
  - [ ] OG image URLs use grab.aesthetic.computer
  - [ ] Twitter card URLs use grab.aesthetic.computer

- [ ] **Redirects Work**
  - [ ] /icon/* requests proxy to grab worker
  - [ ] /preview/* requests proxy to grab worker
  - [ ] Status codes are correct (200)

- [ ] **Social Media Previews**
  - [ ] Discord shows correct preview image
  - [ ] Twitter shows correct preview image
  - [ ] Facebook shows correct preview image
  - [ ] Slack shows correct preview image

- [ ] **Favicons**
  - [ ] Browser tab shows correct favicon
  - [ ] Bookmark shows correct icon
  - [ ] Mobile home screen icon correct

- [ ] **Performance**
  - [ ] First request < 15 seconds
  - [ ] Cached requests < 1 second
  - [ ] No 504 timeouts

- [ ] **Error Handling**
  - [ ] Invalid piece names return 404
  - [ ] Invalid dimensions return 400
  - [ ] Canvas timeout handled gracefully

---

## Git Commit Message

When committing these changes:

```
feat: migrate screenshot generation to Cloudflare Workers

- Update parse.mjs to use grab.aesthetic.computer URLs
- Update netlify.toml redirects to proxy to grab worker
- Add comprehensive documentation in /grab directory
- Maintain backwards compatibility via redirects

Worker deployed at: https://aesthetic-grab.aesthetic-computer.workers.dev
Custom domain pending: grab.aesthetic.computer

Fixes: Screenshot cold starts, improves caching, global CDN
Replaces: /system/netlify/functions/screenshot.js (to be removed after validation)
```

---

## Related Documentation

- **Architecture:** `/grab/README.md`
- **Deployment:** `/grab/DEPLOYMENT.md`
- **Development:** `/grab/DEV-SETUP.md`
- **Migration Plan:** `/grab/MIGRATION.md`
- **Custom Domain:** `/grab/CUSTOM-DOMAIN-SETUP.md`
- **Integration Summary:** `/grab/INTEGRATION-SUMMARY.md`
