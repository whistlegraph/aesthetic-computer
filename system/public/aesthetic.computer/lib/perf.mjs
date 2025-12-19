// perf.mjs - Performance telemetry for Aesthetic Computer
// Tracks timing of boot sequence, network requests, and rendering

const enabled = true; // Set to false to disable all telemetry

// Timing buckets
const timings = {
  boot: {},      // Boot sequence milestones
  network: [],   // Network requests
  glyphs: [],    // Glyph loading batches
  fonts: {},     // Font loading
  render: {},    // First paint, interactive, etc.
};

// Track in-flight requests for concurrency analysis
let inFlightRequests = 0;
let maxConcurrentRequests = 0;
let totalRequests = 0;

// Boot start time (set by bios.mjs)
let bootStart = 0;

// Helper: get time since boot
function sinceBoot() {
  return bootStart ? (performance.now() - bootStart).toFixed(0) : "?";
}

// Mark boot start
export function markBootStart() {
  bootStart = performance.now();
  timings.boot.start = bootStart;
  if (enabled) console.log(`⏱️ [+0ms] Boot started`);
}

// Mark a boot milestone
export function markBoot(name) {
  if (!enabled) return;
  const now = performance.now();
  timings.boot[name] = now;
  console.log(`⏱️ [+${sinceBoot()}ms] 🥾 ${name}`);
}

// Track a network request - two ways to call:
// 1. Simple: trackRequest(url, duration, success) - record timing after the fact
// 2. Advanced: const t = trackRequest(url); ... t.end(status, size) - track start/end
export function trackRequest(url, durationOrOptions = {}, successOrNothing) {
  if (!enabled) return { end: () => {} };
  
  const shortUrl = url.length > 60 ? url.slice(0, 30) + "..." + url.slice(-25) : url;
  
  // Simple mode: duration passed directly
  if (typeof durationOrOptions === 'number') {
    const duration = durationOrOptions;
    const status = successOrNothing ? "ok" : "error";
    console.log(`⏱️ [+${sinceBoot()}ms] 🌐 REQ completed in ${duration.toFixed(0)}ms: ${shortUrl}`);
    
    timings.network.push({
      url: shortUrl,
      duration,
      status,
      size: null,
    });
    totalRequests++;
    return { end: () => {} };
  }
  
  // Advanced mode: track start/end
  const start = performance.now();
  
  inFlightRequests++;
  totalRequests++;
  maxConcurrentRequests = Math.max(maxConcurrentRequests, inFlightRequests);
  
  const reqId = totalRequests;
  console.log(`⏱️ [+${sinceBoot()}ms] 🌐 REQ #${reqId} START (${inFlightRequests} in-flight): ${shortUrl}`);
  
  return {
    end: (status = "ok", size = null) => {
      inFlightRequests--;
      const duration = (performance.now() - start).toFixed(0);
      const sizeStr = size ? ` (${(size/1024).toFixed(1)}KB)` : "";
      console.log(`⏱️ [+${sinceBoot()}ms] 🌐 REQ #${reqId} ${status.toUpperCase()} in ${duration}ms${sizeStr}: ${shortUrl}`);
      
      timings.network.push({
        url: shortUrl,
        duration: parseFloat(duration),
        status,
        size,
      });
    }
  };
}

// Track glyph batch loading - two ways to call:
// 1. Simple: trackGlyphBatch(count, duration, font) - record after the fact
// 2. Advanced: const t = trackGlyphBatch(chars, font); ... t.end(cached, fetched)
export function trackGlyphBatch(countOrChars, durationOrFont, fontOrNothing) {
  if (!enabled) return { end: () => {} };
  
  // Simple mode: count and duration passed directly
  if (typeof countOrChars === 'number' && typeof durationOrFont === 'number') {
    const count = countOrChars;
    const duration = durationOrFont;
    const font = fontOrNothing || "unknown";
    console.log(`⏱️ [+${sinceBoot()}ms] 🔤 GLYPH batch: ${count} chars in ${duration.toFixed(0)}ms (${font})`);
    
    timings.glyphs.push({
      font,
      count,
      cached: 0,
      fetched: count,
      duration,
    });
    return { end: () => {} };
  }
  
  // Advanced mode: track start/end
  const chars = countOrChars;
  const font = durationOrFont;
  const start = performance.now();
  const count = Array.isArray(chars) ? chars.length : chars;
  console.log(`⏱️ [+${sinceBoot()}ms] 🔤 GLYPH batch START: ${count} chars for ${font}`);
  
  return {
    end: (cached = 0, fetched = 0) => {
      const duration = (performance.now() - start).toFixed(0);
      console.log(`⏱️ [+${sinceBoot()}ms] 🔤 GLYPH batch END in ${duration}ms: ${cached} cached, ${fetched} fetched`);
      
      timings.glyphs.push({
        font,
        count,
        cached,
        fetched,
        duration: parseFloat(duration),
      });
    }
  };
}

// Track font loading
export function trackFont(name) {
  if (!enabled) return { end: () => {} };
  
  const start = performance.now();
  console.log(`⏱️ [+${sinceBoot()}ms] 🔠 FONT load START: ${name}`);
  
  return {
    end: () => {
      const duration = (performance.now() - start).toFixed(0);
      console.log(`⏱️ [+${sinceBoot()}ms] 🔠 FONT load END in ${duration}ms: ${name}`);
      timings.fonts[name] = parseFloat(duration);
    }
  };
}

// Mark first paint
export function markFirstPaint() {
  if (!enabled) return;
  timings.render.firstPaint = performance.now();
  console.log(`⏱️ [+${sinceBoot()}ms] 🎨 First paint`);
}

// Mark interactive (all glyphs loaded, UI responsive)
export function markInteractive() {
  if (!enabled) return;
  timings.render.interactive = performance.now();
  console.log(`⏱️ [+${sinceBoot()}ms] ✅ Interactive`);
}

// Print summary report
export function printReport() {
  if (!enabled) return;
  
  const totalTime = performance.now() - bootStart;
  
  console.log(`\n📊 ═══════════════════════════════════════════════════════`);
  console.log(`📊 PERFORMANCE REPORT`);
  console.log(`📊 ═══════════════════════════════════════════════════════`);
  
  // Boot milestones
  console.log(`📊 Boot Milestones:`);
  const bootKeys = Object.keys(timings.boot).filter(k => k !== 'start');
  bootKeys.forEach(key => {
    const ms = (timings.boot[key] - bootStart).toFixed(0);
    console.log(`📊   ${key}: +${ms}ms`);
  });
  
  // Network summary
  console.log(`📊 Network:`);
  console.log(`📊   Total requests: ${totalRequests}`);
  console.log(`📊   Max concurrent: ${maxConcurrentRequests}`);
  if (timings.network.length > 0) {
    const avgDuration = (timings.network.reduce((a, b) => a + b.duration, 0) / timings.network.length).toFixed(0);
    const maxDuration = Math.max(...timings.network.map(r => r.duration)).toFixed(0);
    console.log(`📊   Avg request time: ${avgDuration}ms`);
    console.log(`📊   Max request time: ${maxDuration}ms`);
    
    // Show slowest requests
    const slowest = [...timings.network].sort((a, b) => b.duration - a.duration).slice(0, 5);
    console.log(`📊   Slowest requests:`);
    slowest.forEach(r => {
      console.log(`📊     ${r.duration}ms - ${r.url}`);
    });
  }
  
  // Glyph summary
  if (timings.glyphs.length > 0) {
    console.log(`📊 Glyphs:`);
    const totalGlyphs = timings.glyphs.reduce((a, b) => a + b.count, 0);
    const totalCached = timings.glyphs.reduce((a, b) => a + b.cached, 0);
    const totalFetched = timings.glyphs.reduce((a, b) => a + b.fetched, 0);
    const avgBatchTime = (timings.glyphs.reduce((a, b) => a + b.duration, 0) / timings.glyphs.length).toFixed(0);
    console.log(`📊   Total batches: ${timings.glyphs.length}`);
    console.log(`📊   Total glyphs: ${totalGlyphs} (${totalCached} cached, ${totalFetched} fetched)`);
    console.log(`📊   Avg batch time: ${avgBatchTime}ms`);
  }
  
  // Render timing
  console.log(`📊 Render:`);
  if (timings.render.firstPaint) {
    console.log(`📊   First paint: +${(timings.render.firstPaint - bootStart).toFixed(0)}ms`);
  }
  if (timings.render.interactive) {
    console.log(`📊   Interactive: +${(timings.render.interactive - bootStart).toFixed(0)}ms`);
  }
  
  console.log(`📊 Total time: ${totalTime.toFixed(0)}ms`);
  console.log(`📊 ═══════════════════════════════════════════════════════\n`);
  
  // Expose to window for debugging
  if (typeof window !== 'undefined') {
    window.acPerfTimings = timings;
    window.acPerfReport = printReport;
  }
}

// Expose to window for debugging
if (typeof window !== 'undefined' && enabled) {
  window.acPerfReport = printReport;
}

export default {
  markBootStart,
  markBoot,
  trackRequest,
  trackGlyphBatch,
  trackFont,
  markFirstPaint,
  markInteractive,
  printReport,
};
