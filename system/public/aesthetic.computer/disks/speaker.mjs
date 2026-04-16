// speaker.mjs — interactive ALSA playback device probe.
//
// Each row of the list is one /proc/asound playback PCM the kernel
// advertised. Select with ↑/↓, press space or enter to play a 440 Hz tone
// on the selected device; if something comes out of a speaker or
// headphone, that's the PCM the rest of the system should be using.
// Especially useful on SOF/Chromebook topologies where pcm0 = Speakers vs
// pcm1 = Headset differs per board.
//
// Works on ac-native via `system.audio.listPcms()` / `system.audio.testPcm()`.
// On the web runtime those bindings don't exist — we fall back to a stub
// list so the piece still paints and the user sees it's a native-only
// diagnostic.

let pcms = [];
let selected = 0;
let lastTone = { device: "", freq: 0, ts: 0 };
let activeDevice = "";
let freqs = [100, 200, 440, 880, 1760, 3520];
let freqIdx = 2; // 440 Hz by default
let volumes = [0.1, 0.3, 0.5, 0.8, 1.0];
let volIdx = 3; // 80% — actually audible
let durationsMs = [200, 500, 1000, 2000, 5000];
let durIdx = 1; // 500 ms
let lastMainTone = { freq: 0, vol: 0, ts: 0 };
// Sweep state — uses sim() tick instead of setTimeout (not available in ac-native QuickJS).
let sweepActive = false;
let sweepStartMs = 0;
let sweepFreq = 440;
let sweepStep = 0;

function boot({ system }) {
  refreshPcms(system);
  activeDevice = system?.audio?.activeDevice || "";
  if (activeDevice) {
    const match = pcms.findIndex(p => p.device === activeDevice);
    if (match >= 0) selected = match;
  }
}

function refreshPcms(system) {
  const raw = system?.audio?.listPcms?.();
  if (Array.isArray(raw)) {
    pcms = raw;
  } else {
    // Web fallback: show a single stub so the piece paints something
    // informative when the native API isn't present.
    pcms = [{
      device: "(web stub)",
      card: 0, num: 0,
      id: "native-only",
      name: "system.audio not available on web",
    }];
  }
}

function act({ event: e, system, sound }) {
  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    if (pcms.length) selected = (selected + 1) % pcms.length;
    return;
  }
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    if (pcms.length) selected = (selected - 1 + pcms.length) % pcms.length;
    return;
  }
  if (e.is("keyboard:down:r")) {
    refreshPcms(system);
    return;
  }

  // Frequency cycle: f = next, F / shift+f = prev
  if (e.is("keyboard:down:f")) {
    freqIdx = (freqIdx + 1) % freqs.length;
    return;
  }
  // Volume cycle: v = next
  if (e.is("keyboard:down:v")) {
    volIdx = (volIdx + 1) % volumes.length;
    return;
  }
  // Duration cycle: d = next
  if (e.is("keyboard:down:d")) {
    durIdx = (durIdx + 1) % durationsMs.length;
    return;
  }

  if (e.is("keyboard:down:space") || e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    const p = pcms[selected];
    if (!p || !p.device || p.device === "(web stub)") return;
    const freq = freqs[freqIdx];
    const dur  = durationsMs[durIdx];
    const vol  = volumes[volIdx];
    const ok = system?.audio?.testPcm?.(p.device, freq, dur, vol);
    lastTone = { device: p.device, freq, ts: Date.now(), ok };
    return;
  }

  // 'm' = main audio path test (same path notepat uses).
  // This is the real volume test — if 'm' is too quiet but space on
  // hw:0,0 is loud, the main audio code has wrong gain settings.
  if (e.is("keyboard:down:m")) {
    const freq = freqs[freqIdx];
    const dur  = durationsMs[durIdx] * 0.001;
    const vol  = volumes[volIdx];
    sound?.synth?.({ type: "sine", tone: freq, duration: dur,
                     volume: vol, attack: 0.002, decay: Math.max(0.05, dur * 0.3) });
    lastMainTone = { freq, vol, ts: Date.now() };
    return;
  }

  // 's' = sweep — ramps volume from 20%→100% via sim() tick (no setTimeout).
  if (e.is("keyboard:down:s")) {
    sweepActive = true;
    sweepStartMs = Date.now();
    sweepFreq = freqs[freqIdx];
    sweepStep = 0;
    lastMainTone = { freq: sweepFreq, vol: 1.0, ts: Date.now() };
    return;
  }
}

function sim({ sound }) {
  if (!sweepActive) return;
  const steps = 5;
  const stepMs = 350;
  const elapsed = Date.now() - sweepStartMs;
  const wantStep = Math.floor(elapsed / stepMs);
  while (sweepStep < wantStep && sweepStep < steps) {
    const v = (sweepStep + 1) / steps;
    sound?.synth?.({ type: "sine", tone: sweepFreq, duration: 0.3,
                     volume: v, attack: 0.01, decay: 0.15 });
    sweepStep++;
  }
  if (sweepStep >= steps) sweepActive = false;
}

function paint({ wipe, ink, box, write, screen }) {
  const w = screen.width, h = screen.height;
  const pad = 10;
  const font = "font_1";

  wipe(10, 10, 20);
  ink(255, 255, 180);
  write("speaker probe", { x: pad, y: 10, size: 2, font: "matrix" });

  // Top summary
  ink(140, 160, 180);
  write(`active: ${activeDevice || "none"}`,
        { x: pad, y: 34, size: 1, font });

  ink(200, 200, 100);
  write(`tone: ${freqs[freqIdx]} Hz  vol: ${Math.round(volumes[volIdx] * 100)}%  dur: ${durationsMs[durIdx]} ms`,
        { x: pad, y: 48, size: 1, font });

  // PCM list
  let y = 70;
  ink(80, 180, 200);
  write("playback PCMs:", { x: pad, y, size: 1, font });
  y += 14;

  const rowH = 18;
  const maxRows = Math.min(pcms.length, Math.floor((h - y - 60) / rowH));
  for (let i = 0; i < maxRows; i++) {
    const p = pcms[i];
    const isSel = i === selected;
    const isActive = p.device === activeDevice;
    if (isSel) {
      ink(30, 50, 80);
      box(pad - 2, y - 2, w - pad * 2 + 4, rowH - 2, true);
    }
    ink(isSel ? 255 : 180, isSel ? 255 : 180, isSel ? 255 : 180);
    const marker = isActive ? "●" : " ";
    write(`${marker} ${p.device}`, { x: pad, y, size: 1, font });
    ink(140, 180, 150);
    const idShort = (p.id || "").slice(0, 32);
    write(idShort, { x: pad + 78, y, size: 1, font });
    y += rowH;
  }

  // Last tone status
  if (lastTone.device) {
    const sinceMs = Date.now() - lastTone.ts;
    if (sinceMs < 3000) {
      ink(100, 220, 100);
      write(`▶ ${lastTone.device} @ ${lastTone.freq} Hz`,
            { x: pad, y: h - 50, size: 1, font });
    } else {
      ink(120, 120, 140);
      write(`last: ${lastTone.device} @ ${lastTone.freq} Hz`,
            { x: pad, y: h - 50, size: 1, font });
    }
  }

  // Last main tone status (the "real" volume test — sound.synth goes
  // through the same audio path notepat uses).
  if (lastMainTone.ts) {
    const sinceMs = Date.now() - lastMainTone.ts;
    if (sinceMs < 3000) {
      ink(255, 200, 100);
      write(`M ${lastMainTone.freq} Hz @ vol ${Math.round(lastMainTone.vol * 100)}%`,
            { x: pad, y: h - 32, size: 1, font });
    }
  }

  // Key hints (two lines because we added m/s)
  ink(120, 140, 120);
  write("↑↓ select   space play on PCM   f freq  v vol  d dur",
        { x: pad, y: h - 20, size: 1, font });
  write("m = main audio test   s = sweep 20→100%   r refresh   esc back",
        { x: pad, y: h - 10, size: 1, font });
}

export { boot, paint, act, sim };
