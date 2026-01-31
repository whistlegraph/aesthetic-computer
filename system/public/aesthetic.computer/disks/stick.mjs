// Stick, 2026.1.31.12.00.00 💾
// Render clock melodies to WAV and download

/* #region 📚 README 
  Render audio files from clock melodies.
  
  Usage:
    stick 30 clock ^c,,,,,,   - Render 30s of clock melody and download
    stick 10 tone 440         - Render 10s of 440Hz tone
    
  Click the button or press Enter to render and download!
#endregion */

let duration = 30;
let source = "clock";
let sourceParams = "c";
let status = "Ready";
let rendering = false;
let renderProgress = 0;
let downloadBtn = null;
let trackId = 0;

// Pending render promise
let pendingResolve = null;
let pendingReject = null;

// 🥾 Boot
function boot({ params, screen, resize, num }) {
  resize(320, 180);
  
  // Parse URL params: stick~30~clock~^c,,,,
  if (params.length >= 2) {
    const d = parseFloat(params[0]);
    if (!isNaN(d) && d > 0) {
      duration = d;
      source = params[1]?.toLowerCase() || "clock";
      sourceParams = params.slice(2).join("~") || "c";
    }
  } else if (params.length === 1) {
    // Just duration, default to clock c
    const d = parseFloat(params[0]);
    if (!isNaN(d) && d > 0) duration = d;
  }
  
  status = `${duration}s ${source}: ${sourceParams}`;
}

// 🎨 Paint
function paint({ wipe, ink, screen, ui: { Button }, num }) {
  const { width: w, height: h } = screen;
  wipe(20, 18, 30);
  
  // Title (below HUD area)
  const topY = 20;
  ink("cyan").write("stick", { x: 6, y: topY });
  
  // Track info
  const infoY = topY + 16;
  ink("gray").write("Render:", { x: 6, y: infoY });
  ink("white").write(`${duration}s`, { x: 54, y: infoY });
  ink("gray").write(source, { x: 80, y: infoY });
  ink("lime").write(sourceParams.slice(0, 20), { x: 80 + (source.length + 1) * 6, y: infoY });
  
  // Big download button
  const btnW = Math.min(200, w - 24);
  const btnH = 32;
  const btnX = Math.floor((w - btnW) / 2);
  const btnY = infoY + 24;
  
  downloadBtn = new Button(btnX, btnY, btnW, btnH)
    .paint(({ box, btn }) => {
      const bgColor = rendering 
        ? [60, 60, 80] 
        : btn.down 
          ? [80, 200, 120] 
          : btn.over 
            ? [60, 160, 100] 
            : [40, 120, 80];
      
      ink(...bgColor).box(box);
      ink(80, 100, 90).box(box, "outline");
      
      // Progress bar when rendering
      if (rendering && renderProgress > 0) {
        const progW = Math.floor(box.w * renderProgress);
        ink(100, 220, 140, 150).box(box.x, box.y, progW, box.h);
      }
      
      // Button text
      const label = rendering 
        ? `Rendering ${Math.round(renderProgress * 100)}%...` 
        : "Download WAV";
      const labelX = box.x + Math.floor((box.w - label.length * 6) / 2);
      const labelY = box.y + Math.floor((box.h - 8) / 2);
      ink("white").write(label, { x: labelX, y: labelY });
    });
  
  // Status line
  const statusY = btnY + btnH + 12;
  ink(rendering ? "yellow" : "gray").write(status, { x: 6, y: statusY, width: w - 12 });
  
  // Help text
  const helpY = h - 12;
  ink(50, 50, 70).write("Enter=render  Esc=back", { x: 6, y: helpY });
}

// ✒ Act
async function act({ event: e, download, send, jump, needsPaint }) {
  // Button click
  if (downloadBtn?.act(e, () => startRender(send, download))) {
    needsPaint();
    return;
  }
  
  // Enter key to render
  if (e.is("keyboard:down:enter") && !rendering) {
    startRender(send, download);
    needsPaint();
    return;
  }
  
  // Escape to go back
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

// Start rendering
async function startRender(send, download) {
  if (rendering) return;
  
  rendering = true;
  renderProgress = 0;
  status = "Starting render...";
  trackId++;
  
  try {
    let audioData;
    
    if (source === "tone") {
      // Tones rendered locally
      const freq = parseFloat(sourceParams) || 440;
      audioData = generateToneWAV(freq, duration);
      status = "Tone rendered!";
    } else if (source === "clock") {
      // Clock melodies rendered via bios OfflineAudioContext
      status = "Rendering melody...";
      
      // Create promise for async render
      const renderPromise = new Promise((resolve, reject) => {
        pendingResolve = resolve;
        pendingReject = reject;
        
        // Timeout after 2 minutes
        setTimeout(() => {
          if (pendingReject) {
            pendingReject(new Error("Render timeout"));
            pendingResolve = null;
            pendingReject = null;
          }
        }, 120000);
      });
      
      // Send render request to bios
      send({
        type: "stick:render",
        content: {
          id: trackId,
          source: "clock",
          melody: sourceParams,
          duration: duration,
          sampleRate: 44100
        }
      });
      
      // Wait for bios to complete
      audioData = await renderPromise;
      
      if (!audioData) {
        throw new Error("No audio data returned");
      }
    } else {
      // Unknown source - default to tone
      audioData = generateToneWAV(440, duration);
    }
    
    // Download the file!
    const filename = `${source}-${sourceParams.replace(/[^a-z0-9]/gi, "").slice(0, 16) || "melody"}-${duration}s.wav`;
    download(filename, audioData, { type: "audio/wav" });
    
    status = `Downloaded: ${filename}`;
    
  } catch (err) {
    console.error("Render error:", err);
    status = `Error: ${err.message}`;
  }
  
  rendering = false;
  renderProgress = 1;
}

// 🔔 Receive messages from bios
function receiver({ content }) {
  if (content.type === "stick:render:progress") {
    renderProgress = content.progress;
    status = `Rendering: ${Math.round(content.progress * 100)}%`;
  }
  
  if (content.type === "stick:render:complete") {
    if (pendingResolve) {
      pendingResolve(content.audioData);
      pendingResolve = null;
      pendingReject = null;
    }
  }
  
  if (content.type === "stick:render:error") {
    status = `Error: ${content.error}`;
    rendering = false;
    if (pendingReject) {
      pendingReject(new Error(content.error));
      pendingResolve = null;
      pendingReject = null;
    }
  }
}

// 🔊 Generate WAV file data for a sine tone
function generateToneWAV(frequency, durationSec) {
  const sampleRate = 44100;
  const numChannels = 2;
  const numSamples = Math.floor(sampleRate * durationSec);
  const bytesPerSample = 2;
  const dataSize = numSamples * numChannels * bytesPerSample;
  const buffer = new ArrayBuffer(44 + dataSize);
  const view = new DataView(buffer);
  
  // WAV header
  writeString(view, 0, "RIFF");
  view.setUint32(4, 36 + dataSize, true);
  writeString(view, 8, "WAVE");
  writeString(view, 12, "fmt ");
  view.setUint32(16, 16, true);
  view.setUint16(20, 1, true);
  view.setUint16(22, numChannels, true);
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * numChannels * bytesPerSample, true);
  view.setUint16(32, numChannels * bytesPerSample, true);
  view.setUint16(34, 16, true);
  writeString(view, 36, "data");
  view.setUint32(40, dataSize, true);
  
  // Audio data
  let offset = 44;
  for (let i = 0; i < numSamples; i++) {
    let envelope = 1;
    const fadeLen = sampleRate * 0.01;
    if (i < fadeLen) envelope = i / fadeLen;
    if (i > numSamples - fadeLen) envelope = (numSamples - i) / fadeLen;
    
    const sample = Math.sin(2 * Math.PI * frequency * i / sampleRate) * envelope;
    const intSample = Math.max(-32768, Math.min(32767, sample * 0x7FFF));
    
    view.setInt16(offset, intSample, true);
    offset += 2;
    view.setInt16(offset, intSample, true);
    offset += 2;
  }
  
  return buffer;
}

function writeString(view, offset, string) {
  for (let i = 0; i < string.length; i++) {
    view.setUint8(offset + i, string.charCodeAt(i));
  }
}

export { boot, paint, act, receiver };
