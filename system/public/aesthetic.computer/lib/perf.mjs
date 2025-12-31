// perf.mjs - Performance telemetry for Aesthetic Computer
// Tracks timing of boot sequence, network requests, and rendering

// Disable telemetry in PACK_MODE (NFT bundles) or set to false manually
const enabled = typeof window !== "undefined" && window.acPACK_MODE ? false : true;

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
}

// Mark a boot milestone
export function markBoot(name) {
  if (!enabled) return;
  const now = performance.now();
  timings.boot[name] = now;
  // Silent - use printReport() for summary
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
  
  return {
    end: (status = "ok", size = null) => {
      inFlightRequests--;
      const duration = (performance.now() - start).toFixed(0);
      
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
  
  return {
    end: (cached = 0, fetched = 0) => {
      const duration = (performance.now() - start).toFixed(0);
      
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
  
  return {
    end: () => {
      const duration = (performance.now() - start).toFixed(0);
      timings.fonts[name] = parseFloat(duration);
    }
  };
}

// Mark first paint
export function markFirstPaint() {
  if (!enabled) return;
  timings.render.firstPaint = performance.now();
}

// Mark interactive (all glyphs loaded, UI responsive)
export function markInteractive() {
  if (!enabled) return;
  timings.render.interactive = performance.now();
}

// Print summary report (minimal)
export function printReport() {
  if (!enabled) return;
  
  const totalTime = performance.now() - bootStart;
  const bootKeys = Object.keys(timings.boot).filter(k => k !== 'start');
  const lastMilestone = bootKeys.length > 0 ? bootKeys[bootKeys.length - 1] : 'start';
  
  console.log(`⏱️ Boot: ${totalTime.toFixed(0)}ms (${lastMilestone})`);
  
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
