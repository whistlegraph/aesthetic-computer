# KidLisp.com Dynamic OG Preview Images

## 📋 Overview

Create a dynamic Open Graph image system for KidLisp.com that rotates every 24 hours, showcasing popular community creations. This will make link previews in iMessage, Slack, Discord, Twitter, etc. visually engaging and ever-changing.

## 🎯 Goals

1. **Dynamic Content**: OG image updates every 24 hours based on top hits from the community
2. **Visual Appeal**: Showcase actual KidLisp pieces running, not just static branding
3. **Community Highlights**: Feature popular creators and trending pieces
4. **Caching**: Efficient caching to avoid regenerating images on every request
5. **Fallback**: Graceful degradation if generation fails

---

## 🏗️ Architecture

### Current State

- **give.aesthetic.computer**: Uses static `https://assets.aesthetic.computer/give-og.png?v=3`
- **aesthetic.computer pieces**: Uses `oven.aesthetic.computer/preview/1200x630/{slug}.png` (Puppeteer screenshot)
- **kidlisp.com**: Currently has NO og:image meta tag!
- **oven service**: Already has `/grab/webp/` endpoint for capturing KidLisp GIFs/thumbnails

### Proposed Flow

```
User shares kidlisp.com link
        ↓
iMessage/Slack fetches og:image URL
        ↓
https://oven.aesthetic.computer/kidlisp-og
        ↓
Oven checks Redis cache (24hr TTL)
        ↓
Cache HIT → Return cached image
Cache MISS → Generate new image
        ↓
1. Fetch top hits from /api/tv?types=kidlisp&sort=hits&limit=20
2. Pick "daily feature" piece (deterministic hash of date)
3. Capture screenshot via Puppeteer (3-second animation frame)
4. Composite with KidLisp branding/metadata
5. Cache to DigitalOcean Spaces + Redis
6. Return PNG
```

---

## 💡 Creative Ideas

### Option A: Featured Piece Showcase
A single top-hit piece fills most of the frame, with KidLisp branding overlay:
```
┌─────────────────────────────────────────────────────────────┐
│  ┌───────────────────────────────────────────────────────┐  │
│  │                                                       │  │
│  │           [CAPTURED KIDLISP ANIMATION]               │  │
│  │                    ~3 sec frame                      │  │
│  │                                                       │  │
│  └───────────────────────────────────────────────────────┘  │
│  KidLisp.com                     $featured-code  •  4,634 ▶ │
│  "A simple, expressive language"          by @creator       │
└─────────────────────────────────────────────────────────────┘
```

### Option B: Mosaic/Grid of Top Hits
Show 4-6 pieces in a grid, giving a sense of variety:
```
┌─────────────────────────────────────────────────────────────┐
│  ┌──────────┐ ┌──────────┐ ┌──────────┐                    │
│  │  $bop    │ │  $dance  │ │  $spiral │   KidLisp.com      │
│  │  4.6k ▶  │ │  2.1k ▶  │ │  1.8k ▶  │   ───────────      │
│  └──────────┘ └──────────┘ └──────────┘   Visual code      │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐   for everyone     │
│  │  $pulse  │ │  $wave   │ │  $rain   │                    │
│  │  1.2k ▶  │ │  990 ▶   │ │  850 ▶   │   ⬇ Try now       │
│  └──────────┘ └──────────┘ └──────────┘                    │
└─────────────────────────────────────────────────────────────┘
```

### Option C: Animated Style (Static Frame)
Capture the same piece at multiple time offsets, show as a "filmstrip":
```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│   ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐        │
│   │ t=0  │→ │ t=1  │→ │ t=2  │→ │ t=3  │→ │ t=4  │        │
│   └──────┘  └──────┘  └──────┘  └──────┘  └──────┘        │
│                                                             │
│              $bop  •  4,634 plays  •  KidLisp.com          │
└─────────────────────────────────────────────────────────────┘
```

### Option D: Code + Preview Split
Show the source code alongside the visual output:
```
┌─────────────────────────────────────────────────────────────┐
│  ┌───────────────────────┐ ┌────────────────────────────┐  │
│  │ (wipe "black")        │ │                            │  │
│  │ (repeat 10 i          │ │     [VISUAL OUTPUT]        │  │
│  │   (ink (rainbow i))   │ │                            │  │
│  │   (box (* i 10) ...)) │ │                            │  │
│  └───────────────────────┘ └────────────────────────────┘  │
│           $rainbow-boxes  •  1,234 ▶  •  KidLisp.com       │
└─────────────────────────────────────────────────────────────┘
```

### Option E: Daily Theme Variations
Different visual themes based on day of week:
- **Monday**: Top hit of all time
- **Tuesday**: Random from top 10
- **Wednesday**: Newest piece with >100 hits
- **Thursday**: Featured creator's best work
- **Friday**: Community choice (weighted random)
- **Saturday/Sunday**: "Weekend collection" mosaic

---

## 🛠️ Technical Implementation

### Phase 1: Basic Endpoint

Add to `oven/server.mjs`:
```javascript
// GET /kidlisp-og - Dynamic OG image for kidlisp.com
app.get('/kidlisp-og', async (req, res) => {
  // Check cache first (Redis or Spaces)
  const cachedUrl = await getCachedOGImage();
  if (cachedUrl) {
    return res.redirect(302, cachedUrl);
  }
  
  // Generate new image
  const imageBuffer = await generateKidlispOGImage();
  
  // Upload to Spaces with 24hr TTL
  const url = await uploadToSpaces('kidlisp-og', imageBuffer);
  await setCachedOGImage(url, 24 * 60 * 60);
  
  res.setHeader('Content-Type', 'image/png');
  res.setHeader('Cache-Control', 'public, max-age=86400');
  res.send(imageBuffer);
});
```

### Phase 2: Add to kidlisp.com

Update `system/public/kidlisp.com/index.html` `<head>`:
```html
<!-- Open Graph / Social Preview -->
<meta property="og:type" content="website" />
<meta property="og:url" content="https://kidlisp.com" />
<meta property="og:title" content="KidLisp - Visual Code for Everyone" />
<meta property="og:description" content="A simple, expressive language for creating visual patterns and animations." />
<meta property="og:image" content="https://oven.aesthetic.computer/kidlisp-og" />
<meta property="og:image:width" content="1200" />
<meta property="og:image:height" content="630" />

<!-- Twitter Card -->
<meta name="twitter:card" content="summary_large_image" />
<meta name="twitter:title" content="KidLisp - Visual Code for Everyone" />
<meta name="twitter:description" content="A simple, expressive language for creating visual patterns and animations." />
<meta name="twitter:image" content="https://oven.aesthetic.computer/kidlisp-og" />
```

### Phase 3: Image Generation

Add to `oven/grabber.mjs`:
```javascript
/**
 * Generate KidLisp.com OG preview image
 * Captures a featured piece and composites with branding
 */
export async function generateKidlispOGImage() {
  // 1. Fetch top hits
  const response = await fetch('https://aesthetic.computer/api/tv?types=kidlisp&sort=hits&limit=20');
  const data = await response.json();
  const topPieces = data.media?.kidlisp || [];
  
  // 2. Select daily feature (deterministic based on date)
  const dayOfYear = Math.floor(Date.now() / (24 * 60 * 60 * 1000));
  const featuredIndex = dayOfYear % Math.min(topPieces.length, 10);
  const featured = topPieces[featuredIndex];
  
  // 3. Capture screenshot at 1200x630
  const page = await getBrowserPage();
  await page.setViewport({ width: 1200, height: 630 });
  await page.goto(`https://aesthetic.computer/$${featured.code}?preview=1200x630`, {
    waitUntil: 'networkidle0',
    timeout: 15000
  });
  
  // Wait for animation
  await new Promise(r => setTimeout(r, 3000));
  
  // 4. Screenshot
  const screenshot = await page.screenshot({ type: 'png' });
  
  // 5. Composite with branding (using sharp)
  const composite = await sharp(screenshot)
    .composite([
      { input: await createBrandingOverlay(featured), gravity: 'south' }
    ])
    .png()
    .toBuffer();
  
  await page.close();
  return composite;
}

async function createBrandingOverlay(featured) {
  // Create SVG overlay with text
  const svg = `
    <svg width="1200" height="80">
      <rect width="100%" height="100%" fill="rgba(0,0,0,0.7)"/>
      <text x="20" y="45" font-family="monospace" font-size="24" fill="white">
        KidLisp.com
      </text>
      <text x="1180" y="45" font-family="monospace" font-size="18" fill="#aaa" text-anchor="end">
        $${featured.code} • ${formatHits(featured.hits)} plays ${featured.owner?.handle || ''}
      </text>
    </svg>
  `;
  return Buffer.from(svg);
}
```

### Phase 4: Caching Layer

```javascript
// Redis/Memory cache for OG image URL
const ogImageCache = {
  url: null,
  expires: 0,
};

async function getCachedOGImage() {
  if (ogImageCache.url && Date.now() < ogImageCache.expires) {
    return ogImageCache.url;
  }
  
  // Check Spaces for today's image
  const today = new Date().toISOString().split('T')[0];
  const key = `og/kidlisp/${today}.png`;
  
  try {
    await spacesClient.send(new HeadObjectCommand({
      Bucket: SPACES_BUCKET,
      Key: key,
    }));
    
    const url = `${SPACES_CDN_BASE}/${key}`;
    ogImageCache.url = url;
    ogImageCache.expires = Date.now() + 60 * 60 * 1000; // 1hr memory cache
    return url;
  } catch {
    return null;
  }
}

async function uploadOGImageToSpaces(buffer) {
  const today = new Date().toISOString().split('T')[0];
  const key = `og/kidlisp/${today}.png`;
  
  await spacesClient.send(new PutObjectCommand({
    Bucket: SPACES_BUCKET,
    Key: key,
    Body: buffer,
    ContentType: 'image/png',
    ACL: 'public-read',
    CacheControl: 'public, max-age=86400',
  }));
  
  return `${SPACES_CDN_BASE}/${key}`;
}
```

---

## 📊 Data Sources

### Top Hits API
```bash
curl "https://aesthetic.computer/api/tv?types=kidlisp&sort=hits&limit=20"
```

Returns:
```json
{
  "media": {
    "kidlisp": [
      {
        "code": "bop",
        "hits": 4634,
        "source": "(wipe...)",
        "owner": { "handle": "@jas" },
        "when": "2025-09-15T..."
      },
      ...
    ]
  }
}
```

### Existing Oven Endpoints
- `/grab/webp/{w}/{h}/${code}` - Capture animated WebP
- `/grab/png/{w}/{h}/${code}` - Capture static PNG
- `/preview/{w}x{h}/{piece}.png` - Standard preview (proxied from netlify)

---

## 🚀 Rollout Plan

### Week 1: MVP
1. Add `/kidlisp-og` endpoint to oven
2. Basic single-piece screenshot
3. Simple text overlay with code name + hits
4. Add og:image meta tags to kidlisp.com

### Week 2: Polish
1. Nicer branding overlay (custom font, logo)
2. Add source code snippet preview
3. Redis caching for faster responses
4. Error handling and fallback image

### Week 3: Enhancements
1. Multiple layout options (A-D above)
2. Day-of-week themes (Option E)
3. Dashboard to preview/force-regenerate
4. Analytics on which pieces get shared most

---

## 🔧 Environment Variables Needed

```bash
# Already configured in oven
ART_SPACES_KEY=xxx
ART_SPACES_SECRET=xxx
ART_SPACES_BUCKET=art-aesthetic-computer
MONGODB_CONNECTION_STRING=xxx
```

---

## 📁 Files to Create/Modify

| File | Change |
|------|--------|
| `oven/server.mjs` | Add `/kidlisp-og` route |
| `oven/grabber.mjs` | Add `generateKidlispOGImage()` function |
| `system/public/kidlisp.com/index.html` | Add og:image meta tags |
| `oven/package.json` | Add `sharp` dependency if not present |

---

## 🎨 Design Mockups Needed

- [ ] Option A: Featured piece showcase
- [ ] Option B: Grid/mosaic layout
- [ ] Branding overlay with KidLisp logo
- [ ] Error/fallback static image

---

## ✅ Success Metrics

1. **Link previews work** in iMessage, Slack, Discord, Twitter
2. **Image updates daily** with different featured piece
3. **Response time** < 500ms for cached images
4. **Fallback works** when oven is unavailable

---

## 🔮 Future Ideas

- **Animated OG** (supported by some platforms like Telegram)
- **User-specific previews** for logged-in users sharing their pieces
- **QR code in image** linking directly to the featured piece
- **"Made with KidLisp"** watermark option for shared pieces
- **Seasonal themes** (holiday decorations, etc.)
