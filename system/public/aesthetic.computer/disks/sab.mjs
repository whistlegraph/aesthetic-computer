// sab (SharedArrayBuffer Status), 2025.01.29
// Live system status check - SharedArrayBuffer, embedding context, audio

const { round } = Math;

let reportData = null;
let scrollY = 0;
let lastUpdate = 0;
let netRef = null;
let soundRef = null;

export function boot({ net, sound }) {
  netRef = net;
  soundRef = sound;
  reportData = gatherReport(net, sound);
  lastUpdate = Date.now();
}

export function sim({ simCount, net, sound }) {
  // Refresh every 2 seconds for live status
  if (Date.now() - lastUpdate > 2000) {
    reportData = gatherReport(net, sound);
    lastUpdate = Date.now();
  }
}

function gatherReport(net, sound) {
  const data = {
    timestamp: new Date().toISOString(),
    sections: [],
  };
  
  // 1. SharedArrayBuffer Status
  const sabSection = {
    title: "🔒 SharedArrayBuffer Status",
    items: []
  };
  
  // Check if SAB is available in this worker context
  const sabAvailable = typeof SharedArrayBuffer !== 'undefined';
  
  // net.crossOriginIsolated comes from main thread
  const crossOriginIsolated = net?.crossOriginIsolated === true;
  
  sabSection.items.push({
    label: "SharedArrayBuffer API",
    value: sabAvailable ? "✅ Available" : "❌ Unavailable",
    detail: sabAvailable 
      ? "API is defined in worker context"
      : "API not available (needs COEP/COOP headers)"
  });
  
  sabSection.items.push({
    label: "crossOriginIsolated",
    value: crossOriginIsolated ? "✅ true" : "❌ false",
    detail: crossOriginIsolated 
      ? "Page is cross-origin isolated, SAB usable"
      : "Page NOT isolated - SAB blocked in main thread"
  });
  
  sabSection.items.push({
    label: "Required Headers",
    value: crossOriginIsolated ? "✅ Both set" : "⚠️ Missing",
    detail: "COEP: require-corp, COOP: same-origin"
  });
  
  sabSection.items.push({
    label: "netlify.toml Status",
    value: "COEP commented out",
    detail: "COOP is same-origin-allow-popups (not full isolation)"
  });
  
  sabSection.items.push({
    label: "Audio Use Case",
    value: "Zero-copy worklet transfer",
    detail: "Would allow direct memory sharing w/ AudioWorklet"
  });
  
  data.sections.push(sabSection);
  
  // 2. Audio Info (from sound API)
  const audioSection = {
    title: "🔊 Audio Status",
    items: []
  };
  
  audioSection.items.push({
    label: "Sound API",
    value: sound ? "✅ Available" : "❌ Not ready",
    detail: "Piece sound interface"
  });
  
  if (sound) {
    audioSection.items.push({
      label: "Sample Rate",
      value: sound.sampleRate ? `${sound.sampleRate} Hz` : "Unknown",
      detail: sound.sampleRate >= 48000 ? "High quality" : "Standard"
    });
    
    audioSection.items.push({
      label: "Audio Time",
      value: sound.time ? `${sound.time.toFixed(2)}s` : "N/A",
      detail: "Current audio context time"
    });
  }
  
  data.sections.push(audioSection);
  
  // 3. Embedding Context (from net API)
  const embedSection = {
    title: "🖼️ Embedding Context",
    items: []
  };
  
  embedSection.items.push({
    label: "In iframe",
    value: net?.iframe ? "✅ Yes" : "❌ No",
    detail: net?.iframe ? "Running inside another page" : "Top-level window"
  });
  
  embedSection.items.push({
    label: "Host",
    value: net?.host || "unknown",
    detail: "Current hostname"
  });
  
  // Check for special modes via net
  const packMode = net?.packMode === true;
  const vscodeMode = net?.vscode === true;
  
  embedSection.items.push({
    label: "PACK Mode (NFT)",
    value: packMode ? "✅ Enabled" : "❌ Disabled",
    detail: packMode ? "Running as bundled NFT" : "Normal web mode"
  });
  
  embedSection.items.push({
    label: "VSCode Mode",
    value: vscodeMode ? "✅ Enabled" : "❌ Disabled",
    detail: vscodeMode ? "Running in VSCode extension" : "Browser"
  });
  
  data.sections.push(embedSection);
  
  // 4. Known Embedding Platforms
  const platformsSection = {
    title: "🌐 Supported Platforms",
    items: []
  };
  
  platformsSection.items.push({
    label: "objkt.com",
    value: "Tezos NFT",
    detail: "Sandboxed, no localStorage, PACK mode"
  });
  
  platformsSection.items.push({
    label: "kidlisp.com",
    value: "Code editor",
    detail: "iframe with noauth mode"
  });
  
  platformsSection.items.push({
    label: "Ableton Live",
    value: "DAW M4L",
    detail: "?daw=1 query param"
  });
  
  data.sections.push(platformsSection);
  
  // 5. Headers Info
  const headersSection = {
    title: "📋 Current Headers (netlify.toml)",
    items: []
  };
  
  headersSection.items.push({
    label: "COOP",
    value: "same-origin-allow-popups",
    detail: "Allows popups but NOT full isolation"
  });
  
  headersSection.items.push({
    label: "COEP",
    value: "❌ COMMENTED OUT",
    detail: "Would need 'require-corp' for SAB"
  });
  
  headersSection.items.push({
    label: "CORP",
    value: "cross-origin",
    detail: "Allows cross-origin resource loading"
  });
  
  data.sections.push(headersSection);
  
  // 6. To Enable SAB
  const enableSection = {
    title: "🔧 To Enable SharedArrayBuffer",
    items: []
  };
  
  enableSection.items.push({
    label: "Step 1",
    value: "Uncomment COEP",
    detail: "Cross-Origin-Embedder-Policy = 'require-corp'"
  });
  
  enableSection.items.push({
    label: "Step 2",
    value: "Change COOP",
    detail: "Cross-Origin-Opener-Policy = 'same-origin'"
  });
  
  enableSection.items.push({
    label: "⚠️ Warning",
    value: "Breaks cross-origin",
    detail: "External images/scripts need crossorigin attr"
  });
  
  enableSection.items.push({
    label: "⚠️ Warning",
    value: "Breaks NFT embeds",
    detail: "objkt iframes may fail"
  });
  
  enableSection.items.push({
    label: "Benefit",
    value: "~5ms latency reduction",
    detail: "Zero-copy audio buffer transfer"
  });
  
  data.sections.push(enableSection);
  
  return data;
}

export function paint({ wipe, ink, screen }) {
  wipe(24, 24, 32);
  
  if (!reportData) return;
  
  // Start below the HUD label (which is ~12px tall at top-left)
  let y = 18 - scrollY;
  const lineHeight = 10;
  const labelX = 8;
  const valueX = 160;
  const detailX = labelX;
  
  // Big status indicator at top
  const sabAvailable = typeof SharedArrayBuffer !== 'undefined';
  const crossOriginIsolated = netRef?.crossOriginIsolated === true;
  
  if (crossOriginIsolated && sabAvailable) {
    ink(50, 200, 100).write("✓ CROSS-ORIGIN ISOLATED - SAB ENABLED", { x: labelX, y });
  } else if (sabAvailable) {
    ink(200, 150, 50).write("⚠ SAB API exists but page NOT isolated", { x: labelX, y });
  } else {
    ink(200, 100, 50).write("✗ SharedArrayBuffer DISABLED", { x: labelX, y });
  }
  y += lineHeight * 1.5;
  
  ink(100).write(`Live Status • ${reportData.timestamp}`, { x: labelX, y });
  y += lineHeight * 2;
  
  for (const section of reportData.sections) {
    if (y > screen.height + 20) break;
    if (y > -30) {
      ink(100, 200, 255).write(section.title, { x: labelX, y });
    }
    y += lineHeight * 1.2;
    
    for (const item of section.items) {
      if (y > screen.height + 20) break;
      if (y > -20) {
        ink(180, 180, 180).write(item.label, { x: labelX, y });
        const valueColor = item.value.includes("✅") ? [100, 255, 100] :
                          item.value.includes("❌") ? [255, 100, 100] :
                          item.value.includes("⚠") ? [255, 200, 100] :
                          [255, 255, 255];
        ink(...valueColor).write(item.value, { x: valueX, y });
        y += lineHeight * 0.9;
        ink(100, 100, 120).write(item.detail, { x: detailX + 8, y });
      }
      y += lineHeight * 1.1;
    }
    y += lineHeight * 0.5;
  }
  
  const refreshAgo = round((Date.now() - lastUpdate) / 1000);
  ink(80).write(`↑↓ scroll | r refresh | esc exit | updated ${refreshAgo}s ago`, { x: labelX, y: screen.height - 12 });
}

export function act({ event: e, jump }) {
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
  
  if (e.is("draw")) {
    scrollY = Math.max(0, Math.min(2000, scrollY - e.delta.y * 2));
  }
  
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:q")) {
    jump("prompt");
  }
  
  if (e.is("keyboard:down:r")) {
    reportData = gatherReport(netRef, soundRef);
    lastUpdate = Date.now();
  }
}
