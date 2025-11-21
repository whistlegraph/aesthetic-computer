# Performance Testing Suite

Automated performance testing and optimization tools for aesthetic.computer.

## Quick Start

```bash
# Run basic performance tests
npm run test:perf

# Run Chrome DevTools automated tests
npm run test:perf:chrome

# Run full Lighthouse audit (slower, more comprehensive)
npm run test:perf:lighthouse
```

## What Gets Tested

### 1. Boot Performance (`boot-performance.test.mjs`)
- Boot sequence timing
- BIOS load time
- Disk load time
- Typeface loading optimization (on-demand vs preload)

### 2. Chrome DevTools Tests (`chrome-devtools-test.mjs`)
- **Navigation Timing**: DNS, TCP, TTFB, download
- **Paint Metrics**: First Paint, First Contentful Paint
- **Resource Analysis**: Top 10 slowest resources
- **JavaScript Coverage**: Unused code detection
- **DOM Metrics**: Interactive, Complete timing

### 3. Lighthouse Audit (optional)
- Performance score (0-100)
- Core Web Vitals (FCP, LCP, TBT, CLS)
- Speed Index
- Detailed recommendations

## Performance Targets

| Metric | Target | Critical |
|--------|--------|----------|
| Total Boot Time | < 2s | < 5s |
| First Contentful Paint | < 1s | < 2s |
| Time to Interactive | < 3s | < 5s |
| JavaScript Coverage | > 60% | > 40% |

## Output

Test results are logged to console and saved to `./reports/`:
- `lighthouse-{timestamp}.json` - Full Lighthouse reports
- Console output shows real-time metrics

## CI/CD Integration

Tests run automatically on:
- Every push to `main`
- All pull requests
- Manual workflow dispatch

See `.github/workflows/performance-tests.yml` for configuration.

## Environment Variables

```bash
# Set test URL (default: https://localhost:8888)
TEST_URL=https://aesthetic.computer npm run test:perf:chrome

# Enable/disable Lighthouse (default: false for chrome test, true for lighthouse test)
RUN_LIGHTHOUSE=true npm run test:perf:chrome
```

## Recent Optimizations

✅ **Typeface Preload Removal** - Saved ~6 seconds on boot
- Changed from preloading Unifont to on-demand loading
- Stub typeface created instantly
- Glyphs load as needed

✅ **Caddy Server Optimization** - Improved dev server response
- HTTP/2 support
- Better compression (zstd + gzip)
- No-cache headers for instant updates

## Adding New Tests

1. Create test file in `tests/performance/`
2. Add npm script to `package.json`
3. Update this README
4. Update `.github/workflows/performance-tests.yml` if needed

## Troubleshooting

**"Connection refused" errors**
- Make sure dev server is running (`npm run site`)
- Check TEST_URL matches your server

**Lighthouse fails**
- Requires stable connection
- May need to disable in CI with `RUN_LIGHTHOUSE=false`

**Coverage seems low**
- Normal for initial load (modules load on-demand)
- Check unused code with DevTools → Coverage tab

## Resources

- [Chrome DevTools Performance](https://developer.chrome.com/docs/devtools/performance/)
- [Lighthouse Documentation](https://developer.chrome.com/docs/lighthouse/)
- [Performance API](https://developer.mozilla.org/en-US/docs/Web/API/Performance)
- [Puppeteer](https://pptr.dev/)
