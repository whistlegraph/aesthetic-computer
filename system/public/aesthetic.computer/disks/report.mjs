// report, 2025.01.29.00.00
// System resources and embedding context report

const { abs, round, floor } = Math;

let reportData = null;
let scrollY = 0;

export function boot({ screen }) {
  // Gather all the data on boot
  reportData = gatherReport();
}

function gatherReport() {
  const data = {
    timestamp: new Date().toISOString(),
    sections: [],
  };
  
  // 1. SharedArrayBuffer Status
  const sharedArrayBufferSection = {
    title: "🔒 SharedArrayBuffer Status",
    items: []
  };
  
  const sabAvailable = typeof SharedArrayBuffer !== 'undefined';
  const crossOriginIsolated = typeof window !== 'undefined' && window.crossOriginIsolated === true;
  
  sharedArrayBufferSection.items.push({
    label: "SharedArrayBuffer API",
    value: sabAvailable ? "✅ Available" : "❌ Unavailable",
    detail: sabAvailable 
      ? "API is defined in this context"
      : "API not available (needs COEP/COOP headers)"
  });
  
  sharedArrayBufferSection.items.push({
    label: "crossOriginIsolated",
    value: crossOriginIsolated ? "✅ true" : "❌ false",
    detail: crossOriginIsolated 
      ? "Page is cross-origin isolated, SAB can be used"
      : "Page NOT isolated - SAB will be blocked"
  });
  
  // Check actual COEP/COOP headers we're getting
  sharedArrayBufferSection.items.push({
    label: "Required Headers",
    value: crossOriginIsolated ? "✅ Both set" : "⚠️ Missing",
    detail: "COEP: require-corp, COOP: same-origin"
  });
  
  sharedArrayBufferSection.items.push({
    label: "Current Config",
    value: "netlify.toml",
    detail: "COEP is COMMENTED OUT, COOP is same-origin-allow-popups"
  });
  
  sharedArrayBufferSection.items.push({
    label: "Use Case",
    value: "AudioWorklet zero-copy transfer",
    detail: "Would allow direct memory sharing between main thread and worklet"
  });
  
  data.sections.push(sharedArrayBufferSection);
  
  // 2. AudioContext Info
  const audioSection = {
    title: "🔊 Audio Context",
    items: []
  };
  
  if (typeof window !== 'undefined' && window.audioContext) {
    const ctx = window.audioContext;
    audioSection.items.push({
      label: "State",
      value: ctx.state,
      detail: ctx.state === "running" ? "Audio is active" : "Audio suspended/closed"
    });
    audioSection.items.push({
      label: "Sample Rate",
      value: `${ctx.sampleRate} Hz`,
      detail: ctx.sampleRate >= 48000 ? "High quality" : "Standard quality"
    });
    audioSection.items.push({
      label: "Base Latency",
      value: ctx.baseLatency ? `${(ctx.baseLatency * 1000).toFixed(2)} ms` : "N/A",
      detail: "Minimum latency of audio subsystem"
    });
    audioSection.items.push({
      label: "Output Latency",
      value: ctx.outputLatency ? `${(ctx.outputLatency * 1000).toFixed(2)} ms` : "N/A",
      detail: "Actual output device latency"
    });
  } else {
    audioSection.items.push({
      label: "Status",
      value: "❌ Not initialized",
      detail: "AudioContext not yet created"
    });
  }
  
  data.sections.push(audioSection);
  
  // 3. Embedding Context
  const embedSection = {
    title: "🖼️ Embedding Context",
    items: []
  };
  
  const isIframe = window !== window.top;
  const origin = window.origin;
  const sandboxed = origin === "null";
  const packMode = window.acPACK_MODE === true;
  const vscodeMode = window.acVSCODE === true;
  const noAuthMode = window.acNOAUTH === true;
  
  embedSection.items.push({
    label: "In iframe",
    value: isIframe ? "✅ Yes" : "❌ No",
    detail: isIframe ? "Running inside another page" : "Top-level window"
  });
  
  embedSection.items.push({
    label: "Origin",
    value: sandboxed ? "null (sandboxed)" : origin,
    detail: sandboxed ? "Sandboxed iframe - limited access" : "Normal origin"
  });
  
  embedSection.items.push({
    label: "PACK Mode (NFT)",
    value: packMode ? "✅ Enabled" : "❌ Disabled",
    detail: packMode ? "Running as bundled NFT" : "Normal web mode"
  });
  
  embedSection.items.push({
    label: "VSCode Mode",
    value: vscodeMode ? "✅ Enabled" : "❌ Disabled",
    detail: vscodeMode ? "Running in VSCode extension" : "Normal browser"
  });
  
  embedSection.items.push({
    label: "NoAuth Mode",
    value: noAuthMode ? "✅ Enabled" : "❌ Disabled",
    detail: noAuthMode ? "Auth skipped for embedding" : "Normal auth flow"
  });
  
  // Storage access
  let localStorageOk = false;
  let sessionStorageOk = false;
  try {
    localStorage.setItem("__test", "1");
    localStorage.removeItem("__test");
    localStorageOk = true;
  } catch {}
  try {
    sessionStorage.setItem("__test", "1");
    sessionStorage.removeItem("__test");
    sessionStorageOk = true;
  } catch {}
  
  embedSection.items.push({
    label: "localStorage",
    value: localStorageOk ? "✅ Available" : "❌ Blocked",
    detail: localStorageOk ? "Persistent storage works" : "Sandboxed iframe blocks this"
  });
  
  embedSection.items.push({
    label: "sessionStorage",
    value: sessionStorageOk ? "✅ Available" : "❌ Blocked",
    detail: sessionStorageOk ? "Session storage works" : "Sandboxed iframe blocks this"
  });
  
  data.sections.push(embedSection);
  
  // 4. Known Embedding Platforms
  const platformsSection = {
    title: "🌐 Embedding Platforms",
    items: []
  };
  
  platformsSection.items.push({
    label: "objkt.com",
    value: "NFT marketplace",
    detail: "Sandboxed iframe, no localStorage, PACK mode"
  });
  
  platformsSection.items.push({
    label: "OpenSea",
    value: "NFT marketplace",
    detail: "Sandboxed iframe, no localStorage, PACK mode"
  });
  
  platformsSection.items.push({
    label: "fxhash",
    value: "Generative art",
    detail: "Sandboxed iframe, PACK mode with $fx object"
  });
  
  platformsSection.items.push({
    label: "teia.art",
    value: "NFT marketplace",
    detail: "Sandboxed iframe, similar to objkt"
  });
  
  platformsSection.items.push({
    label: "kidlisp.com",
    value: "Code editor",
    detail: "iframe with noauth mode, theme detection"
  });
  
  platformsSection.items.push({
    label: "Ableton Live",
    value: "DAW integration",
    detail: "M4L device with ?daw=1 query param"
  });
  
  platformsSection.items.push({
    label: "VSCode Extension",
    value: "Editor panel",
    detail: "acVSCODE mode, full feature access"
  });
  
  data.sections.push(platformsSection);
  
  // 5. Current Headers (from netlify.toml)
  const headersSection = {
    title: "📋 Production Headers",
    items: []
  };
  
  headersSection.items.push({
    label: "Access-Control-Allow-Origin",
    value: "*",
    detail: "Wide open CORS for asset loading"
  });
  
  headersSection.items.push({
    label: "Cross-Origin-Opener-Policy",
    value: "same-origin-allow-popups",
    detail: "Allows popups but NOT full isolation"
  });
  
  headersSection.items.push({
    label: "Cross-Origin-Resource-Policy",
    value: "cross-origin",
    detail: "Allows resources to be loaded cross-origin"
  });
  
  headersSection.items.push({
    label: "Cross-Origin-Embedder-Policy",
    value: "❌ COMMENTED OUT",
    detail: "Would need 'require-corp' for SharedArrayBuffer"
  });
  
  headersSection.items.push({
    label: "Cache-Control",
    value: "no-cache, no-store",
    detail: "No caching for fresh deploys"
  });
  
  data.sections.push(headersSection);
  
  // 6. Enabling SharedArrayBuffer - Requirements
  const enableSabSection = {
    title: "🔧 To Enable SharedArrayBuffer",
    items: []
  };
  
  enableSabSection.items.push({
    label: "Step 1",
    value: "Uncomment COEP header",
    detail: "Cross-Origin-Embedder-Policy = 'require-corp'"
  });
  
  enableSabSection.items.push({
    label: "Step 2",
    value: "Change COOP header",
    detail: "Cross-Origin-Opener-Policy = 'same-origin'"
  });
  
  enableSabSection.items.push({
    label: "⚠️ Breaking Change",
    value: "External resources",
    detail: "All cross-origin images/scripts need crossorigin='anonymous'"
  });
  
  enableSabSection.items.push({
    label: "⚠️ Breaking Change",
    value: "iframes",
    detail: "External iframes would need allow='cross-origin-isolated'"
  });
  
  enableSabSection.items.push({
    label: "⚠️ Breaking Change",
    value: "CDN assets",
    detail: "assets.aesthetic.computer needs CORS headers"
  });
  
  enableSabSection.items.push({
    label: "Benefit",
    value: "Zero-copy audio transfer",
    detail: "~5-10ms latency reduction for audio worklet"
  });
  
  data.sections.push(enableSabSection);
  
  // 7. Memory & Performance
  const perfSection = {
    title: "💾 Memory & Performance",
    items: []
  };
  
  if (performance.memory) {
    const mem = performance.memory;
    perfSection.items.push({
      label: "JS Heap Used",
      value: `${(mem.usedJSHeapSize / 1024 / 1024).toFixed(1)} MB`,
      detail: `of ${(mem.totalJSHeapSize / 1024 / 1024).toFixed(1)} MB total`
    });
    perfSection.items.push({
      label: "JS Heap Limit",
      value: `${(mem.jsHeapSizeLimit / 1024 / 1024).toFixed(0)} MB`,
      detail: "Maximum allocatable"
    });
  } else {
    perfSection.items.push({
      label: "Memory API",
      value: "❌ Not available",
      detail: "Chrome-only feature"
    });
  }
  
  perfSection.items.push({
    label: "Device Memory",
    value: navigator.deviceMemory ? `${navigator.deviceMemory} GB` : "N/A",
    detail: "Approximate device RAM"
  });
  
  perfSection.items.push({
    label: "Hardware Concurrency",
    value: navigator.hardwareConcurrency ? `${navigator.hardwareConcurrency} cores` : "N/A",
    detail: "Logical CPU cores"
  });
  
  data.sections.push(perfSection);
  
  return data;
}

export function paint({ wipe, ink, write, screen, typeface }) {
  wipe(24, 24, 32);
  
  if (!reportData) return;
  
  let y = 10 - scrollY;
  const lineHeight = 10;
  const labelX = 8;
  const valueX = 160;
  const detailX = labelX;
  
  ink(255, 200, 100).write("System Resources Report", { x: labelX, y });
  y += lineHeight * 1.5;
  
  ink(100).write(reportData.timestamp, { x: labelX, y });
  y += lineHeight * 2;
  
  for (const section of reportData.sections) {
    if (y > screen.height + 20) break; // Stop rendering off-screen
    if (y > -30) { // Only render if in view
      ink(100, 200, 255).write(section.title, { x: labelX, y });
    }
    y += lineHeight * 1.2;
    
    for (const item of section.items) {
      if (y > screen.height + 20) break;
      if (y > -20) {
        // Label
        ink(180, 180, 180).write(item.label, { x: labelX, y });
        // Value
        const valueColor = item.value.includes("✅") ? [100, 255, 100] :
                          item.value.includes("❌") ? [255, 100, 100] :
                          item.value.includes("⚠️") ? [255, 200, 100] :
                          [255, 255, 255];
        ink(...valueColor).write(item.value, { x: valueX, y });
        y += lineHeight * 0.9;
        // Detail (smaller, dimmer)
        ink(100, 100, 120).write(item.detail, { x: detailX + 8, y });
      }
      y += lineHeight * 1.1;
    }
    y += lineHeight * 0.5;
  }
  
  // Scroll hint at bottom
  ink(80).write("↑↓ scroll | esc to exit", { x: labelX, y: screen.height - 12 });
}

export function act({ event: e, jump }) {
  // Scroll handling
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    scrollY = Math.min(scrollY + 30, 2000);
  }
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    scrollY = Math.max(scrollY - 30, 0);
  }
  if (e.is("keyboard:down:pagedown") || e.is("keyboard:down:space")) {
    scrollY = Math.min(scrollY + 200, 2000);
  }
  if (e.is("keyboard:down:pageup")) {
    scrollY = Math.max(scrollY - 200, 0);
  }
  if (e.is("keyboard:down:home")) {
    scrollY = 0;
  }
  if (e.is("keyboard:down:end")) {
    scrollY = 2000;
  }
  
  // Touch/mouse scroll
  if (e.is("draw")) {
    scrollY = Math.max(0, Math.min(2000, scrollY - e.delta.y * 2));
  }
  
  // Exit
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:q")) {
    jump("prompt");
  }
}

export { boot, paint, act };
