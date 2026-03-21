# Performance Optimization Guide

## ✅ Recent Improvements (Nov 2024)

### Typeface Preload Removal - **Saved ~6 seconds** 🚀
- **Before:** 5689ms to preload Unifont + MatrixChunky8
- **After:** 0ms (instant stub, on-demand loading)
- **Impact:** Boot time reduced by ~75% in development

### Development Server Optimization
- **Caddy**: HTTP/2, zstd compression, no-cache headers
- **Fast reload**: Instant updates during development

### Chrome DevTools Integration
- **Automated testing**: Puppeteer + Lighthouse
- **Performance tracking**: CI/CD integration
- **Coverage analysis**: Identify unused code

## Boot Performance Improvements

### 1. Typeface Preloading ✅ OPTIMIZED
**Before:** ~6 seconds to preload Unifont + MatrixChunky8
**After:** Instant (on-demand loading)

Changed in `system/public/aesthetic.computer/lib/disk.mjs`:
- Set `skipTypefacePreload = true` (always)
- Glyphs load on-demand when needed
- Saves 5-6 seconds on every boot

### 2. Auth0 Loading
Currently loads asynchronously but can block ready state.
Consider:
- Lazy load auth0 after initial render
- Use web workers for auth operations
- Cache auth state more aggressively

### 3. Development Server Optimization

#### Caddy (Port 8111) - OPTIMIZED ✅
```caddyfile
:8111 {
  protocols h1 h2c          # HTTP/2 support
  encode zstd gzip          # Better compression
  header Cache-Control "no-store"  # No cache in dev
}
```

#### Netlify Dev
Consider adding to `netlify.toml`:
```toml
[dev]
  targetPort = 8111
  framework = "#static"
  autoLaunch = false
  
[dev.processing]
  skip_processing = true  # Skip build processing in dev
```

## Performance Testing

### Run Tests
```bash
# Basic performance test
npm run test:perf

# Chrome DevTools automated test
npm run test:perf:chrome

# Full Lighthouse audit (slower)
npm run test:perf:lighthouse
```

### Chrome DevTools Automated Testing ✅ INTEGRATED

The `chrome-devtools-test.mjs` script provides:
- **Performance Metrics**: DNS, TCP, TTFB, FCP, DOM timings
- **Resource Analysis**: Identifies slowest-loading resources
- **JavaScript Coverage**: Shows unused code percentage
- **Lighthouse Audits**: Performance scoring and recommendations

Example output:
```
⏱️  Performance Metrics:
  Total Load Time: 2847ms
  First Contentful Paint: 847ms
  DOM Complete: 2456ms
  
📦 Top 10 Slowest Resources:
  1. boot.mjs - 1234ms (156KB)
  2. bios.mjs - 876ms (234KB)
  
📊 JavaScript Coverage:
  Coverage: 67.3%
  Unused: 142KB
```

### Chrome DevTools Integration

#### 1. Performance Timeline
Open Chrome DevTools → Performance tab
- Start recording before page load
- Look for:
  - Script evaluation time
  - Network requests (especially fonts)
  - Long tasks (>50ms)
  - Layout shifts

#### 2. Coverage Analysis
DevTools → More Tools → Coverage
- Shows unused JavaScript/CSS
- Helps identify code splitting opportunities

#### 3. Network Throttling
DevTools → Network → Throttling
- Test with "Fast 3G" or "Slow 3G"
- Identifies slow resources

#### 4. Lighthouse
DevTools → Lighthouse
- Run performance audit
- Focus on:
  - Time to Interactive
  - First Contentful Paint
  - Total Blocking Time

### Chrome DevTools MCP (Future)
Consider integrating:
- `@modelcontextprotocol/server-puppeteer` for automated testing
- Chrome Performance API for programmatic metrics
- Puppeteer for headless performance testing

Example:
```javascript
import puppeteer from 'puppeteer';

const browser = await puppeteer.launch();
const page = await browser.newPage();

// Enable Performance monitoring
await page.tracing.start({ screenshots: true });
await page.goto('https://localhost:8888');

// Wait for boot
await page.waitForFunction(() => window.acBOOT_COMPLETE);

const trace = await page.tracing.stop();
// Analyze trace for bottlenecks
```

## Current Bottlenecks (as of Nov 2024)

1. ~~Typeface preload: 5689ms~~ ✅ FIXED
2. Auth0 initialization: ~500-1000ms
3. MatrixChunky8 preload: ~14ms (now skipped)
4. Network latency in dev containers

## Metrics to Track

- **Boot Start to Ready**: Target <2s
- **Boot Start to First Paint**: Target <500ms
- **Disk Load Time**: Target <1s
- **TypeScript/Module Evaluation**: Target <300ms

## Future Optimizations

- [ ] Code splitting for pieces (dynamic imports)
- [ ] Service worker caching
- [ ] Brotli compression in production
- [ ] HTTP/3 support
- [ ] WebAssembly for compute-heavy pieces
- [ ] IndexedDB caching for fonts/assets
- [ ] Resource hints (preconnect, dns-prefetch)
