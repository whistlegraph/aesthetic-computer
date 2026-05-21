// fx-demos.mjs — interactive demos for pop/dance/synths/fx.mjs ring
// modulation and vocoder effects. Loaded by pop/demos.html via its
// MODULES list; exports a `demos` object consumed by the shell.
//
// Both demos mutate a *copy* of the pre-built source buffer — the
// original stays clean so re-triggering is cheap and alloc-free.

import * as kit from "../lib/demo-kit.mjs";
import { applyRingMod, applyVocoder, softClip } from "../dance/synths/fx.mjs";

// ── 1. ring modulation ────────────────────────────────────────────────

function mountRingMod(root) {
  // Pre-build the dry source once at mount time (2 s sawtooth at 165 Hz).
  const source = kit.shape(kit.osc(165, 2.0, { type: "saw", gain: 0.5 }));

  // Controls.
  const freqSlider  = kit.slider("freq", { min: 20, max: 1500, step: 1, value: 220, unit: " Hz" });
  const mixSlider   = kit.slider("mix",  { min: 0,  max: 1,    step: 0.01, value: 1 });
  const waveRow     = kit.picker("waveform", ["sine", "tri", "square"], "sine");

  // Waveform canvas.
  const cv = kit.el("canvas");

  // Initialise the canvas with the dry waveform so there's something to
  // look at before the user clicks play.
  kit.plot(cv, {
    layers: [{ data: source, color: "#8a7f66", mode: "wave" }],
  });

  // Play button.
  const playBtn = kit.button("▶ play", async () => {
    await kit.resume();
    kit.stopAll();

    const clone = source.slice();
    applyRingMod(clone, {
      freq:      freqSlider.value,
      waveform:  waveRow.value,
      mix:       mixSlider.value,
      sampleRate: kit.SR,
    });

    kit.play(clone);

    kit.plot(cv, {
      layers: [{ data: clone, color: "#e8a05a", mode: "wave" }],
    });
  });

  // Layout.
  root.append(
    kit.el("div", { class: "demo-row" }, freqSlider, mixSlider, waveRow),
    kit.el("div", { class: "demo-row" }, playBtn),
    cv,
  );
}

// ── 2. vocoder ────────────────────────────────────────────────────────

function mountVocoder(root) {
  // Controls.
  const bandsSlider  = kit.slider("bands",   { min: 4, max: 24, step: 1, value: 16, unit: " bands" });
  const carrierPicker = kit.picker("carrier", ["saw", "square", "noise"], "saw");

  const cv  = kit.el("canvas");
  const stat = kit.status("ready — record your voice or use the demo modulator.");

  // Build a carrier buffer matching the given length (samples).
  function buildCarrier(nSamples, type) {
    const durSec = nSamples / kit.SR;
    if (type === "noise") {
      return kit.shape(kit.noise(durSec, { gain: 0.6 }), { attack: 0.02, release: 0.08 });
    }
    // Saw/square: a small chord at 110/165/220 Hz for fullness.
    const t = type === "square" ? "square" : "saw";
    const c1 = kit.osc(110, durSec, { type: t, gain: 0.28 });
    const c2 = kit.osc(165, durSec, { type: t, gain: 0.22 });
    const c3 = kit.osc(220, durSec, { type: t, gain: 0.18 });
    return kit.shape(kit.mix(c1, c2, c3), { attack: 0.02, release: 0.08 });
  }

  // Build a synthetic speech-like modulator (no mic required). Two
  // tones amplitude-modulated by slow sine envelopes at slightly
  // different rates — gives rhythmic spectral energy vaguely like
  // syllables when vocoded.
  function buildSyntheticModulator() {
    const durSec = 2.5;
    const n = Math.floor(durSec * kit.SR);
    const out = new Float32Array(n);
    // Two formant-ish tones with independent amplitude LFOs.
    for (let i = 0; i < n; i++) {
      const t = i / kit.SR;
      // "vowel" bodies: 400 Hz + 1200 Hz
      const tone1 = Math.sin(2 * Math.PI * 400  * t);
      const tone2 = Math.sin(2 * Math.PI * 1200 * t) * 0.5;
      // Slow AM envelopes that pulse at different rates (2.7 Hz / 3.9 Hz)
      // to mimic the rhythm of connected speech.
      const env1 = 0.5 + 0.5 * Math.sin(2 * Math.PI * 2.7 * t);
      const env2 = 0.5 + 0.5 * Math.sin(2 * Math.PI * 3.9 * t + 1.1);
      out[i] = (tone1 * env1 + tone2 * env2) * 0.35;
    }
    kit.shape(out, { attack: 0.03, release: 0.12 });
    return out;
  }

  // Core process-and-play: takes a ready modulator Float32Array.
  function processAndPlay(modulator) {
    const carrier = buildCarrier(modulator.length, carrierPicker.value);
    // applyVocoder mutates the carrier in place — use the carrier copy directly.
    applyVocoder(carrier, {
      modulator,
      bands:      Math.round(bandsSlider.value),
      sampleRate: kit.SR,
    });
    softClip(carrier, 1.2);

    kit.play(carrier);

    kit.plot(cv, {
      layers: [
        { data: modulator, color: "#8a7f66", mode: "wave" },
        { data: carrier,   color: "#e8a05a", mode: "wave" },
      ],
    });
    stat.say("playing ↑ modulator (grey) · vocoded output (amber)");
  }

  // "demo" button — no mic required.
  const demoBtn = kit.button("▶ demo (no mic)", async () => {
    await kit.resume();
    kit.stopAll();
    stat.say("generating synthetic modulator…");
    const mod = buildSyntheticModulator();
    stat.say("vocoding…");
    processAndPlay(mod);
  });

  // "record" button — uses the mic for 3 seconds.
  const recBtn = kit.button("⏺ record 3s & vocode", async () => {
    await kit.resume();
    kit.stopAll();
    recBtn.disabled = true;
    demoBtn.disabled = true;
    stat.say("recording… (speak now)");
    try {
      const mod = await kit.recordMic(3, (progress) => {
        const pct = Math.round(progress * 100);
        stat.say(`recording… ${pct}%`);
      });
      stat.say("vocoding…");
      processAndPlay(mod);
    } catch (err) {
      stat.say(`mic error: ${err.message} — try the demo button instead.`);
    } finally {
      recBtn.disabled = false;
      demoBtn.disabled = false;
    }
  });

  // Initialise canvas placeholder so it isn't just empty black.
  const placeholder = kit.osc(110, 0.1, { type: "saw", gain: 0.4 });
  kit.plot(cv, {
    layers: [{ data: placeholder, color: "#8a7f66", mode: "wave" }],
  });

  // Layout.
  root.append(
    kit.el("div", { class: "demo-row" }, bandsSlider, carrierPicker),
    kit.el("div", { class: "demo-row" }, demoBtn, recBtn),
    stat,
    cv,
  );
}

// ── exports ───────────────────────────────────────────────────────────

export const demos = {
  "ringmod": {
    title:  "ringmod",
    blurb:  "multiply a sawtooth by a carrier — low freqs tremolo, audio-rate freqs add clangorous sidebands.",
    accent: "#c0631a",
    mount:  mountRingMod,
  },
  "vocoder": {
    title:  "vocoder",
    blurb:  "a synth carrier 'speaks' the spectrum of your voice (or a synthetic modulator).",
    accent: "#c0631a",
    mount:  mountVocoder,
  },
};
