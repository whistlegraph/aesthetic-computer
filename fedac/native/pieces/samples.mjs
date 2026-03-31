// samples.mjs — Sample library manager for AC Native
// Lists saved audio samples from /mnt/samples/, plays them back,
// and saves new snapshots of the current sample buffer.
// Jumped to from prompt.mjs via "samples" command.

const SAMPLES_DIR = "/mnt/samples";
let samples = []; // { name, path, size, date }
let selectedIdx = 0;
let frame = 0;
let message = "";
let messageFrame = 0;
let storageInfo = "";

function scanSamples(system) {
  samples = [];
  // Read manifest file that we maintain
  const manifest = system?.readFile?.(`${SAMPLES_DIR}/manifest.json`);
  if (manifest) {
    try { samples = JSON.parse(manifest); } catch {}
  }
}

function saveManifest(system) {
  system?.writeFile?.(`${SAMPLES_DIR}/manifest.json`, JSON.stringify(samples, null, 2));
}

function boot({ system }) {
  scanSamples(system);

  // Storage info
  const bootDev = system?.bootDevice || "?";
  storageInfo = `${bootDev} -> ${SAMPLES_DIR}`;
}

function act({ event: e, sound, system }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }

  // Navigate
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:tab")) {
    if (samples.length > 0) {
      selectedIdx = (selectedIdx + 1) % samples.length;
      sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.025 });
    }
    return;
  }
  if (e.is("keyboard:down:arrowup")) {
    if (samples.length > 0) {
      selectedIdx = (selectedIdx - 1 + samples.length) % samples.length;
      sound?.synth({ type: "sine", tone: 440, duration: 0.03, volume: 0.08, attack: 0.002, decay: 0.025 });
    }
    return;
  }

  // Save current sample as timestamped file
  if (e.is("keyboard:down:s")) {
    const mic = sound?.microphone || {};
    const len = mic.sampleLength || 0;
    if (len <= 0) {
      message = "no sample loaded";
      messageFrame = frame;
      sound?.synth({ type: "square", tone: 220, duration: 0.1, volume: 0.06, attack: 0.005, decay: 0.08 });
      return;
    }
    // Generate timestamp filename
    const now = new Date();
    const ts = [
      now.getFullYear(),
      String(now.getMonth() + 1).padStart(2, "0"),
      String(now.getDate()).padStart(2, "0"),
      "-",
      String(now.getHours()).padStart(2, "0"),
      String(now.getMinutes()).padStart(2, "0"),
      String(now.getSeconds()).padStart(2, "0"),
    ].join("");
    const path = `${SAMPLES_DIR}/${ts}.raw`;
    const saved = sound?.sample?.saveTo?.(path) || -1;
    if (saved > 0) {
      const secs = (mic.sampleRate > 0) ? (saved / mic.sampleRate).toFixed(2) : "?";
      message = `saved ${secs}s -> ${ts}.raw`;
      sound?.synth({ type: "triangle", tone: 784, duration: 0.08, volume: 0.12, attack: 0.003, decay: 0.06 });
      // Add to manifest
      samples.unshift({ name: ts, path, size: saved * 4 + 8, rate: mic.sampleRate || 0, len: saved });
      saveManifest(system);
      selectedIdx = 0;
    } else {
      message = "save failed";
      sound?.synth({ type: "square", tone: 220, duration: 0.1, volume: 0.06, attack: 0.005, decay: 0.08 });
    }
    messageFrame = frame;
    return;
  }

  // Load selected sample
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (samples.length > 0 && samples[selectedIdx]) {
      const s = samples[selectedIdx];
      const loaded = sound?.sample?.loadFrom?.(s.path) || -1;
      if (loaded > 0) {
        const mic = sound?.microphone || {};
        const secs = (mic.sampleRate > 0) ? (loaded / mic.sampleRate).toFixed(2) : "?";
        message = `loaded ${s.name} (${secs}s)`;
        sound?.synth({ type: "triangle", tone: 523, duration: 0.06, volume: 0.1, attack: 0.003, decay: 0.05 });
      } else {
        message = "load failed";
        sound?.synth({ type: "square", tone: 220, duration: 0.1, volume: 0.06, attack: 0.005, decay: 0.08 });
      }
      messageFrame = frame;
    }
    return;
  }

  // Play preview of selected sample
  if (e.is("keyboard:down:space")) {
    if (samples.length > 0 && samples[selectedIdx]) {
      // Load and play at C4
      const s = samples[selectedIdx];
      sound?.sample?.loadFrom?.(s.path);
      sound?.sample?.play?.({ tone: 261.63, base: 261.63, volume: 0.7, attack: 0.01, decay: 0.5 });
      sound?.synth({ type: "sine", tone: 261.63, duration: 0.02, volume: 0.03, attack: 0.001, decay: 0.015 });
    }
    return;
  }

  // Delete selected sample
  if (e.is("keyboard:down:delete") || e.is("keyboard:down:x")) {
    if (samples.length > 0 && samples[selectedIdx]) {
      const s = samples[selectedIdx];
      // Remove from manifest (file stays on disk but is unlisted)
      samples.splice(selectedIdx, 1);
      saveManifest(system);
      message = `removed ${s.name}`;
      messageFrame = frame;
      if (selectedIdx >= samples.length) selectedIdx = Math.max(0, samples.length - 1);
      sound?.synth({ type: "sine", tone: 330, duration: 0.06, volume: 0.08, attack: 0.003, decay: 0.05 });
    }
    return;
  }
}

function paint({ wipe, ink, box, write, screen, sound, system }) {
  frame++;
  const T = __theme.update();
  const w = screen.width, h = screen.height;
  const pad = 10;
  const font = "font_1";

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  // Title
  ink(T.fg, T.fg + 10, T.fg);
  write("samples", { x: pad, y: 10, size: 2, font: "matrix" });

  // Storage location
  ink(T.fgMute, T.fgMute, T.fgMute + 10);
  write(storageInfo, { x: pad, y: 34, size: 1, font });

  // Current sample info
  const mic = sound?.microphone || {};
  const curLen = mic.sampleLength || 0;
  const curRate = mic.sampleRate || 0;
  if (curLen > 0) {
    const secs = curRate > 0 ? (curLen / curRate).toFixed(2) : "?";
    ink(80, 200, 120);
    write(`active: ${secs}s @ ${curRate}Hz`, { x: pad, y: 46, size: 1, font });
  } else {
    ink(T.fgMute);
    write("no sample loaded", { x: pad, y: 46, size: 1, font });
  }

  // Message
  if (message && frame - messageFrame < 180) {
    const fade = Math.max(0, 255 - (frame - messageFrame) * 2);
    ink(200, 255, 180, fade);
    write(message, { x: pad, y: 58, size: 1, font });
  }

  // Sample list
  const listY = 74;
  const rowH = 14;
  const maxRows = Math.floor((h - listY - 40) / rowH);

  if (samples.length === 0) {
    ink(T.fgMute);
    write("no saved samples", { x: pad, y: listY, size: 1, font });
    ink(T.fgMute - 20, T.fgMute, T.fgMute + 10);
    write("record in notepat, then press s here", { x: pad, y: listY + 14, size: 1, font });
  } else {
    // Scroll window
    let startIdx = 0;
    if (selectedIdx >= maxRows) startIdx = selectedIdx - maxRows + 1;

    for (let i = startIdx; i < Math.min(samples.length, startIdx + maxRows); i++) {
      const s = samples[i];
      const ry = listY + (i - startIdx) * rowH;
      const selected = i === selectedIdx;

      if (selected) {
        ink(30, 50, 60);
        box(pad - 2, ry - 1, w - pad * 2 + 4, rowH - 2, true);
      }

      ink(selected ? 255 : T.fgMute, selected ? 255 : T.fgMute, selected ? 255 : T.fgMute);
      write((selected ? "> " : "  ") + s.name, { x: pad, y: ry, size: 1, font });

      // Duration
      if (s.len && s.rate) {
        ink(80, 80, 100);
        const secs = (s.len / s.rate).toFixed(1);
        write(secs + "s", { x: w - pad - 24, y: ry, size: 1, font });
      } else if (s.size) {
        ink(80, 80, 100);
        const kb = (s.size / 1024).toFixed(0);
        write(kb + "KB", { x: w - pad - 36, y: ry, size: 1, font });
      }
    }

    // Scroll indicator
    if (samples.length > maxRows) {
      ink(60, 60, 80);
      write(`${selectedIdx + 1}/${samples.length}`, { x: w - pad - 30, y: listY - 12, size: 1, font });
    }
  }

  // Controls
  const ctrlY = h - 26;
  ink(80, 80, 100);
  write("s:save  enter:load  space:play  x:del", { x: pad, y: ctrlY, size: 1, font });
  ink(T.fgMute, T.fgMute + 10, T.fgMute);
  write("esc: back", { x: pad, y: ctrlY + 12, size: 1, font });
}

function sim() {}

export { boot, paint, act, sim };
