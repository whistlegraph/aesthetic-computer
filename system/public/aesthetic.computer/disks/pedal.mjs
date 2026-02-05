// Pedal, 2026.2.05
// Audio effect pedal for Ableton Live - processes incoming audio through Web Audio effects.

/* 📝 Notes
  - Audio samples streamed from Max via acSample() calls
  - Samples processed through BIOS effects chain (filter, delay)
  - Processed audio output goes back to Ableton via jweb~ audio outputs
  
  Technical Architecture:
  - Max streams samples at ~1kHz via executejavascript -> window.acSample(l, r)
  - Samples collected into buffers, played through ScriptProcessorNode
  - BIOS effects chain processes the audio
  - jweb~ captures Web Audio output -> plugout~
*/

// === State ===
let dawMode = false;
let isEffectMode = false; // effect=1 query param means we receive samples
let dawSynced = false;
let dawBpm = 120;
let dawPlaying = false;

// Incoming sample buffer (ring buffer for streaming)
const SAMPLE_BUFFER_SIZE = 4096;
let sampleBufferL = new Float32Array(SAMPLE_BUFFER_SIZE);
let sampleBufferR = new Float32Array(SAMPLE_BUFFER_SIZE);
let sampleWriteIndex = 0;
let sampleReadIndex = 0;
let samplesReceived = 0;
let sampleBufferReady = false;

// Effect parameters (sent to BIOS)
let effectMode = "lowpass"; // passthrough, lowpass, highpass, echo
const effectModes = ["passthrough", "lowpass", "highpass", "echo"];
let effectModeIndex = 1;

// Filter parameters
let filterCutoff = 2000; // Hz
let filterQ = 1.0;

// Delay parameters  
let delayTime = 0.25; // seconds
let delayFeedback = 0.4;
let wetMix = 0.3;

// Effect resend timer
let lastEffectSendAt = 0;

// Envelope data from Max (for visualization)
let envelope = {
  peakL: 0,
  peakR: 0,
};
let envelopeSmoothed = {
  peakL: 0,
  peakR: 0,
};
const ENV_SMOOTHING = 0.85;
const ENV_ATTACK = 0.4;

// Visual state
let peakHistory = [];
const PEAK_HISTORY_LENGTH = 100;

// Stats
let hasBooted = false;
let peakReceiveCount = 0;
let effectMessagesSent = 0;
let lastEffectAck = null;
let testToneActive = false;

// Worker send function
let sendToBios = null;

// UI Buttons
let modeBtn = null;
let cutoffUpBtn = null;
let cutoffDownBtn = null;
let paramBtn = null;
let wetSlider = null;
let testToneBtn = null;

// === Boot ===
function boot({ query, hud, send, ui, screen }) {
  dawMode = query?.daw === "1" || query?.daw === 1;
  isEffectMode = query?.effect === "1" || query?.effect === 1;
  sendToBios = send;

  if (dawMode) {
    // Default to an obvious echo in DAW mode so the effect is clearly audible.
    effectMode = "echo";
    effectModeIndex = effectModes.indexOf("echo");
    delayTime = 0.6;
    delayFeedback = 0.6;
    wetMix = 0.75;
  }
  
  // Set up sample receiving from Max
  if (isEffectMode && typeof window !== "undefined") {
    // Global function that Max calls via executejavascript
    window.acSample = (l, r) => {
      // Write to ring buffer
      sampleBufferL[sampleWriteIndex] = l;
      sampleBufferR[sampleWriteIndex] = r;
      sampleWriteIndex = (sampleWriteIndex + 1) % SAMPLE_BUFFER_SIZE;
      samplesReceived++;
      
      // Mark buffer ready once we have enough samples
      if (!sampleBufferReady && samplesReceived > 256) {
        sampleBufferReady = true;
        console.log("🎸 Sample buffer ready, starting playback");
        // Tell BIOS to start the sample playback source
        send({ type: "pedal:start-sample-source" });
      }
    };
    
    // Global function for reading samples (called from BIOS AudioWorklet)
    window.acReadSamples = (count) => {
      const outputL = new Float32Array(count);
      const outputR = new Float32Array(count);
      
      for (let i = 0; i < count; i++) {
        outputL[i] = sampleBufferL[sampleReadIndex];
        outputR[i] = sampleBufferR[sampleReadIndex];
        sampleReadIndex = (sampleReadIndex + 1) % SAMPLE_BUFFER_SIZE;
      }
      
      return { left: outputL, right: outputR };
    };
    
    console.log("🎸 Effect mode enabled - waiting for samples from Max");
  }
  
  if (!hasBooted) {
    console.log("🎸 Pedal boot - DAW mode:", dawMode);
    hasBooted = true;
  }
  
  // Avoid HUD label so top-left stays clear for buttons.
  
  // Build UI buttons
  buildButtons({ ui, screen });
  
  // Send initial effect state to BIOS after a short delay
  setTimeout(() => updateBiosEffect(), 100);
  
  // Initialize peak history
  for (let i = 0; i < PEAK_HISTORY_LENGTH; i++) {
    peakHistory.push(0);
  }
}

// Build UI buttons
function buildButtons({ ui, screen }) {
  const w = screen.width;
  const h = screen.height;
  const compact = h < 200;
  
  if (compact) {
    // === COMPACT LAYOUT (DAW mode) ===
    // Row 1: [MODE] [SLIDER----------] [TEST]
    // Row 2: [-][param][+]
    const btnH = 14;
    const row1Y = 14; // Push down to avoid HUD label
    const row2Y = row1Y + btnH + 2;
    
    // Mode button (left, offset from corner)
    const modeW = 32;
    modeBtn = new ui.Button(32, row1Y, modeW, btnH);
    
    // Test button (right)
    const testW = 20;
    testToneBtn = new ui.Button(w - testW - 2, row1Y, testW, btnH);
    
    // Slider (middle, between mode and test)
    const sliderX = 32 + modeW + 4;
    const sliderW = w - sliderX - testW - 6;
    wetSlider = new ui.Slider(sliderX, row1Y + 2, sliderW, btnH - 4, { min: 0, max: 1, value: wetMix });
    
    // Row 2: [-][value][+]
    const stepW = 20;
    const paramW = 40;
    const r2Total = stepW + paramW + stepW + 4;
    const r2X = Math.round((w - r2Total) / 2);
    
    cutoffDownBtn = new ui.Button(r2X, row2Y, stepW, btnH);
    paramBtn = new ui.Button(r2X + stepW + 2, row2Y, paramW, btnH);
    cutoffUpBtn = new ui.Button(r2X + stepW + 2 + paramW + 2, row2Y, stepW, btnH);
    
  } else {
    // === NORMAL LAYOUT ===
    const btnHeight = 16;
    const btnY = 4;
    const btnGap = 4;

    const modeW = 70;
    const stepW = 24;
    const paramW = 60;
    const totalW = modeW + stepW + stepW + paramW + btnGap * 3;
    const startX = Math.max(32, Math.round((w - totalW) / 2));

    // Mode button (cycles through effect modes)
    modeBtn = new ui.Button(startX, btnY, modeW, btnHeight);

    // Cutoff/param down button
    cutoffDownBtn = new ui.Button(startX + modeW + btnGap, btnY, stepW, btnHeight);

    // Cutoff/param up button
    cutoffUpBtn = new ui.Button(startX + modeW + btnGap + stepW + btnGap, btnY, stepW, btnHeight);

    // Parameter display button (shows current value)
    paramBtn = new ui.Button(startX + modeW + btnGap + stepW + btnGap + stepW + btnGap, btnY, paramW, btnHeight);

    // Dry/Wet slider (centered)
    const sliderW = Math.min(160, w - 40);
    const sliderH = 10;
    const sliderX = Math.round((w - sliderW) / 2);
    const sliderY = Math.round(h * 0.75);
    wetSlider = new ui.Slider(sliderX, sliderY, sliderW, sliderH, { min: 0, max: 1, value: wetMix });
    
    // Test tone button (bottom right, in status bar area)
    testToneBtn = new ui.Button(w - 40, h - 18, 36, 14);
  }
}

// Send current effect parameters to BIOS
function updateBiosEffect() {
  // Compute filter parameters
  let filterType = "lowpass";
  let filterFreq = 20000; // No filtering by default

  if (effectMode === "lowpass") {
    filterType = "lowpass";
    filterFreq = filterCutoff;
  } else if (effectMode === "highpass") {
    filterType = "highpass";
    filterFreq = filterCutoff;
  } else if (effectMode === "passthrough") {
    filterType = "lowpass";
    filterFreq = 20000; // Wide open
  }

  const isEcho = effectMode === "echo";
  const delayWet = isEcho ? wetMix : 0;
  const delayMs = delayTime * 1000; // Convert to ms for Max
  
  // Send to Max (for native DSP processing)
  if (typeof window !== "undefined" && window.max?.outlet) {
    window.max.outlet("effect", "filter", filterType, filterFreq);
    window.max.outlet("effect", "delay", delayMs);
    window.max.outlet("effect", "wet", delayWet);
    window.max.outlet("effect", "feedback", delayFeedback);
  }
  
  // Also send to BIOS (for local testing without Max)
  if (sendToBios) {
    sendToBios({
      type: "pedal:effect",
      content: {
        filterType,
        filterFreq,
        filterQ,
        delayTime,
        delayFeedback,
        wetMix: delayWet,
      }
    });
  }
  
  effectMessagesSent++;
  console.log(`🎸 Effect update #${effectMessagesSent}: ${effectMode}, cutoff=${filterFreq}Hz, wet=${delayWet}`);
}

// === Sim ===
function sim({ sound }) {
  // Smooth envelope values
  for (const key of ["peakL", "peakR"]) {
    const target = envelope[key];
    const current = envelopeSmoothed[key];
    if (target > current) {
      envelopeSmoothed[key] = current * ENV_ATTACK + target * (1 - ENV_ATTACK);
    } else {
      envelopeSmoothed[key] = current * ENV_SMOOTHING + target * (1 - ENV_SMOOTHING);
    }
  }
  
  // Update peak history for waveform display
  peakHistory.push(envelopeSmoothed.peakL);
  if (peakHistory.length > PEAK_HISTORY_LENGTH) {
    peakHistory.shift();
  }
  
  // Update DAW state
  if (sound.daw?.bpm) {
    dawSynced = true;
    dawBpm = sound.daw.bpm;
    dawPlaying = sound.daw?.playing ?? false;
  }

  // Keep BIOS in sync in DAW mode (helps when audio init is late)
  if (dawMode && performance.now() - lastEffectSendAt > 1500) {
    updateBiosEffect();
    lastEffectSendAt = performance.now();
  }
}

// === Paint ===
function paint({ wipe, ink, screen, line, box, write }) {
  const w = screen.width;
  const h = screen.height;
  
  // Compact mode for short heights (like DAW embeds)
  const compact = h < 200;
  
  // Background
  const bgBrightness = effectMode === "passthrough" ? 40 : 20;
  wipe(bgBrightness);
  
  // Mode colors
  const modeColors = {
    passthrough: [80, 80, 80],
    lowpass: [60, 60, 200],
    highpass: [200, 60, 60],
    echo: [60, 200, 60],
  };
  
  if (compact) {
    // === COMPACT LAYOUT (DAW mode, short height) ===
    // Single row: [MODE] [SLIDER-------] [TEST]
    const btnH = 14;
    const row1Y = 2;
    const row2Y = row1Y + btnH + 2;
    
    // Mode button
    modeBtn?.paint((btn) => {
      ink(...(modeColors[effectMode] || [100, 100, 100])).box(btn.box);
      const label = effectMode === "echo" ? "ECHO" : effectMode.slice(0, 4).toUpperCase();
      ink(255).write(label, { x: btn.box.x + 2, y: btn.box.y + 3 });
    });
    
    // Dry/Wet slider (most of the width)
    if (wetSlider) {
      const { x, y, w: sw, h: sh } = wetSlider.box;
      const t = wetSlider.normalized;
      ink(40).box(x, y, sw, sh);
      ink(0, 180, 200).box(x, y, Math.floor(sw * t), sh);
      ink(220).box(x + Math.floor(sw * t) - 1, y, 3, sh);
    }
    
    // Test button
    testToneBtn?.paint((btn) => {
      ink(testToneActive ? [0, 200, 100] : [60, 60, 60]).box(btn.box);
      ink(255).write(testToneActive ? "■" : "▶", { x: btn.box.x + 4, y: btn.box.y + 3 });
    });
    
    // +/- buttons
    cutoffDownBtn?.paint((btn) => {
      ink(60).box(btn.box);
      ink(255).write("-", { x: btn.box.x + 5, y: btn.box.y + 3 });
    });
    cutoffUpBtn?.paint((btn) => {
      ink(60).box(btn.box);
      ink(255).write("+", { x: btn.box.x + 5, y: btn.box.y + 3 });
    });
    
    // Param display
    paramBtn?.paint((btn) => {
      ink(40).box(btn.box);
      let txt = effectMode === "echo" ? `${Math.round(wetMix*100)}%` : `${Math.round(filterCutoff)}`;
      ink(200).write(txt, { x: btn.box.x + 2, y: btn.box.y + 3 });
    });
    
    // Waveform in remaining space
    const waveY = row2Y + btnH + 2;
    const waveH = h - waveY - 14;
    if (waveH > 10) {
      ink(25).box(0, waveY, w, waveH);
      ink(0, 200, 100);
      for (let i = 1; i < PEAK_HISTORY_LENGTH; i++) {
        const x1 = ((i - 1) / PEAK_HISTORY_LENGTH) * w;
        const x2 = (i / PEAK_HISTORY_LENGTH) * w;
        const p1 = Math.sqrt(peakHistory[i - 1] || 0);
        const p2 = Math.sqrt(peakHistory[i] || 0);
        const y1 = waveY + (1 - p1) * waveH;
        const y2 = waveY + (1 - p2) * waveH;
        line(x1, y1, x2, y2);
      }
    }
    
    // Status line at bottom
    ink(dawMode ? [0, 180, 80] : [180, 80, 0]).write(
      dawMode ? "DAW" : "LOC", { x: 2, y: h - 10 }
    );
    ink(120).write(`FX:${effectMessagesSent}`, { x: 30, y: h - 10 });
    
  } else {
    // === NORMAL LAYOUT (taller screens) ===
    
    // Mode button
    modeBtn?.paint((btn) => {
      ink(...(modeColors[effectMode] || [100, 100, 100])).box(btn.box);
      const modeLabel = effectMode === "echo" ? "ECHO" : effectMode.slice(0, 6).toUpperCase();
      ink(255).write(modeLabel, { x: btn.box.x + 4, y: btn.box.y + 4 });
    });
    
    // Down button
    cutoffDownBtn?.paint((btn) => {
      ink(70).box(btn.box);
      ink(255).write("-", { x: btn.box.x + 8, y: btn.box.y + 4 });
    });
    
    // Up button
    cutoffUpBtn?.paint((btn) => {
      ink(70).box(btn.box);
      ink(255).write("+", { x: btn.box.x + 8, y: btn.box.y + 4 });
    });
    
    // Parameter display
    paramBtn?.paint((btn) => {
      ink(50).box(btn.box);
      let paramText = "";
      if (effectMode === "lowpass" || effectMode === "highpass") {
        paramText = `${Math.round(filterCutoff)}Hz`;
      } else if (effectMode === "echo") {
        paramText = `${Math.round(wetMix * 100)}%`;
      } else {
        paramText = "---";
      }
      ink(255).write(paramText, { x: btn.box.x + 4, y: btn.box.y + 4 });
    });
    
    // Waveform
    const waveH = h * 0.3;
    const waveY = 28;
    ink(25).box(0, waveY, w, waveH);
    ink(0, 200, 100);
    for (let i = 1; i < PEAK_HISTORY_LENGTH; i++) {
      const x1 = ((i - 1) / PEAK_HISTORY_LENGTH) * w;
      const x2 = (i / PEAK_HISTORY_LENGTH) * w;
      const p1 = Math.sqrt(peakHistory[i - 1] || 0);
      const p2 = Math.sqrt(peakHistory[i] || 0);
      const y1 = waveY + (1 - p1) * waveH;
      const y2 = waveY + (1 - p2) * waveH;
      line(x1, y1, x2, y2);
    }
    
    // Peak Meter
    const meterH = h * 0.08;
    const meterY = h * 0.55;
    const meterW = w * 0.8;
    const meterX = w * 0.1;
    ink(60).box(meterX, meterY, meterW, meterH);
    const peakColor = envelopeSmoothed.peakL > 0.8 ? [255, 50, 50] : [0, 255, 100];
    ink(...peakColor).box(meterX, meterY, meterW * envelopeSmoothed.peakL, meterH);

    // Dry/Wet Slider
    if (wetSlider) {
      const { x, y, w: sw, h: sh } = wetSlider.box;
      const t = wetSlider.normalized;
      ink(50).box(x, y, sw, sh);
      ink(0, 180, 200).box(x, y, Math.floor(sw * t), sh);
      ink(220).box(x + Math.floor(sw * t) - 2, y - 2, 4, sh + 4);
      ink(160).write("DRY", { x: x - 24, y: y - 1 });
      ink(160).write("WET", { x: x + sw + 4, y: y - 1 });
    }
    
    // Status bar
    const statusY = h - 16;
    ink(30).box(0, statusY, w, 16);
    ink(dawMode ? [0, 200, 100] : [200, 100, 0]).write(
      dawMode ? "DAW" : "LOCAL", { x: 4, y: statusY + 3 }
    );
    ink(150).write(`FX:${effectMessagesSent} PK:${peakReceiveCount}`, { x: 50, y: statusY + 3 });
    
    // Test button
    testToneBtn?.paint((btn) => {
      ink(testToneActive ? [0, 200, 100] : [80, 80, 80]).box(btn.box);
      ink(255).write(testToneActive ? "OFF" : "TEST", { x: btn.box.x + 2, y: btn.box.y + 3 });
    });
  }
}

// === Act ===
function act({ event }) {
  // Dry/Wet slider (scrub)
  wetSlider?.act(event, {
    change: (slider) => {
      if (effectMode !== "echo") {
        effectMode = "echo";
        effectModeIndex = effectModes.indexOf("echo");
      }
      wetMix = slider.value;
      updateBiosEffect();
    },
  });

  // Mode button - cycle through effect modes
  modeBtn?.act(event, () => {
    effectModeIndex = (effectModeIndex + 1) % effectModes.length;
    effectMode = effectModes[effectModeIndex];
    console.log("🎸 Effect mode:", effectMode);
    updateBiosEffect();
  });
  
  // Down button - decrease parameter
  cutoffDownBtn?.act(event, () => {
    if (effectMode === "lowpass" || effectMode === "highpass") {
      filterCutoff = Math.max(20, filterCutoff / 1.2);
      updateBiosEffect();
    } else if (effectMode === "echo") {
      wetMix = Math.max(0, wetMix - 0.1);
      wetSlider?.setValue(wetMix);
      updateBiosEffect();
    }
  });
  
  // Up button - increase parameter
  cutoffUpBtn?.act(event, () => {
    if (effectMode === "lowpass" || effectMode === "highpass") {
      filterCutoff = Math.min(20000, filterCutoff * 1.2);
      updateBiosEffect();
    } else if (effectMode === "echo") {
      wetMix = Math.min(1.0, wetMix + 0.1);
      wetSlider?.setValue(wetMix);
      updateBiosEffect();
    }
  });

  // Cycle through effect modes with space
  if (event.is("keyboard:down: ")) {
    effectModeIndex = (effectModeIndex + 1) % effectModes.length;
    effectMode = effectModes[effectModeIndex];
    console.log("🎸 Effect mode:", effectMode);
    updateBiosEffect();
  }
  
  // Adjust filter cutoff with up/down
  if (event.is("keyboard:down:up")) {
    if (effectMode === "lowpass" || effectMode === "highpass") {
      filterCutoff = Math.min(20000, filterCutoff * 1.2);
      updateBiosEffect();
    } else if (effectMode === "echo") {
      wetMix = Math.min(1.0, wetMix + 0.1);
      wetSlider?.setValue(wetMix);
      updateBiosEffect();
    }
  }
  
  if (event.is("keyboard:down:down")) {
    if (effectMode === "lowpass" || effectMode === "highpass") {
      filterCutoff = Math.max(20, filterCutoff / 1.2);
      updateBiosEffect();
    } else if (effectMode === "echo") {
      wetMix = Math.max(0, wetMix - 0.1);
      wetSlider?.setValue(wetMix);
      updateBiosEffect();
    }
  }
  
  // Adjust delay time/feedback with left/right
  if (event.is("keyboard:down:left")) {
    if (effectMode === "echo") {
      delayTime = Math.max(0.01, delayTime - 0.05);
      updateBiosEffect();
    }
  }
  
  if (event.is("keyboard:down:right")) {
    if (effectMode === "echo") {
      delayTime = Math.min(2.0, delayTime + 0.05);
      updateBiosEffect();
    }
  }
  
  // Test tone button - toggle a test sound
  testToneBtn?.act(event, () => {
    testToneActive = !testToneActive;
    if (testToneActive) {
      // Play a test tone via sound system
      sendToBios?.({
        type: "sound-effect",
        content: {
          type: "sine",
          tone: 440,
          duration: 10000, // 10 seconds
          attack: 0.01,
          decay: 0.1,
          volume: 0.3,
        }
      });
      console.log("🎸 Test tone ON");
    } else {
      // Kill all sounds
      sendToBios?.({ type: "sound:kill" });
      console.log("🎸 Test tone OFF");
    }
  });
  
  // T key also toggles test tone
  if (event.is("keyboard:down:t")) {
    testToneActive = !testToneActive;
    if (testToneActive) {
      sendToBios?.({
        type: "sound-effect", 
        content: {
          type: "sine",
          tone: 440,
          duration: 10000,
          attack: 0.01,
          decay: 0.1,
          volume: 0.3,
        }
      });
    } else {
      sendToBios?.({ type: "sound:kill" });
    }
  }
}

// === Receive (messages from main thread / M4L) ===
function receive(event) {
  if (event.type === "pedal:peak") {
    envelope.peakL = event.peak;
    envelope.peakR = event.peak;
    peakReceiveCount++;
    // Log occasionally to confirm data flow
    if (peakReceiveCount <= 3 || peakReceiveCount % 100 === 0) {
      console.log("🎸 Peak:", event.peak?.toFixed(3), "count:", peakReceiveCount);
    }
  } else if (event.type === "pedal:envelope") {
    envelope.peakL = event.peakL || 0;
    envelope.peakR = event.peakR || 0;
  }
}

// === Meta ===
function meta() {
  return {
    title: "Pedal",
    desc: "Audio effect pedal for Ableton Live",
  };
}

export { boot, sim, paint, act, receive, meta };
