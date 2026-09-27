// analyze.mjs — relative pitch analysis for the MODEM sample collection.
//
// For each .mp3 in this directory: decode to mono f32 @ 48 kHz via ffmpeg,
// Goertzel-scan 80–3600 Hz in 4 Hz steps over up to 3 s windows (skipping
// the first 0.3 s; long handshakes get 2–3 windows at different offsets
// since they change character over time — dial tone, answer tone, carrier).
// Records the top spectral peaks per window (merged within 40 Hz), the
// nearest MIDI note + cents for each, and rate multipliers that land the
// strongest stable peak on E-world targets (E2..E5, E-minor-friendly).
//
//   node pop/samples/modem/analyze.mjs        → writes manifest.json
//
// Re-runnable; pure function of the .mp3s present.

import { execSync } from "node:child_process";
import { readdirSync, writeFileSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48000;
const SCAN_LO = 80, SCAN_HI = 3600, SCAN_STEP = 4; // Hz
const WINDOW_SEC = 3, SKIP_SEC = 0.3;
const MERGE_HZ = 40; // peaks closer than this collapse into the stronger one
const TOP_PEAKS = 5;
const TONAL_RATIO = 50; // strongest peak ≥ 50× median band power ⇒ tonal

// E-world targets for an E-rooted composition (FEM bells, E power chords).
const TARGETS = {
  E2: 82.41, B2: 123.47, E3: 164.81, G3: 196.0, B3: 246.94,
  E4: 329.63, G4: 392.0, B4: 493.88, E5: 659.26,
};
// Fallback for carriers pitched too high to reach E2–E5 within a
// [0.5, 2.0] rate: higher E-minor chord tones, used only when the
// primary table yields nothing.
const HIGH_TARGETS = { G5: 783.99, B5: 987.77, E6: 1318.51 };

// Character tags, curated per sample (Freesound id → tags). The tonal/noisy
// flag comes from the scan itself; these describe what the recording *is*.
const TAGS = {
  8037: ["clicks"],                       // Modem#1.53 — short data burst
  8055: ["clicks"],                       // Modem#1.97 — short data burst
  16475: ["handshake", "dialing"],        // classic full dial-up sequence
  42996: ["carrier", "mixed"],            // alien fax squeal
  49608: ["handshake", "dialing"],        // dialup login, Dec 2001
  62843: ["handshake"],
  78657: ["carrier"],                     // 1200 baud carrier
  109143: ["carrier"],                    // BPSK125
  109145: ["carrier"],                    // BPSK31
  109147: ["carrier"],                    // RTTY 45 baud @ 1000 Hz
  158621: ["mixed", "dialing", "handshake"],
  188828: ["handshake", "dialing"],       // 56k modem dial
  397079: ["clicks"],                     // digital radio data noise
  454649: ["handshake"],                  // MODEM_3
  454650: ["handshake"],                  // MODEM_2
  454651: ["handshake"],                  // MODEM_1
  586442: ["mixed"],                      // fake dial-up, tape recorded
  591324: ["carrier", "handshake"],       // VARA FM "hello world"
  658932: ["handshake", "dialing"],
  844723: ["disconnect", "clicks"],       // AT&T gateway disconnect/reconnect
};

function decode(file) {
  const raw = execSync(
    `ffmpeg -hide_banner -loglevel error -i ${JSON.stringify(file)} ` +
    `-ac 1 -ar ${SR} -f f32le -`,
    { maxBuffer: 512 * 1024 * 1024 },
  );
  return new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
}

// Goertzel power at one frequency over samples[start..start+n).
function goertzel(samples, start, n, hz) {
  const w = (2 * Math.PI * hz) / SR;
  const coeff = 2 * Math.cos(w);
  let s0 = 0, s1 = 0, s2 = 0;
  const end = start + n;
  for (let i = start; i < end; i++) {
    s0 = samples[i] + coeff * s1 - s2;
    s2 = s1;
    s1 = s0;
  }
  return (s1 * s1 + s2 * s2 - coeff * s1 * s2) / n; // normalized power
}

function scanWindow(samples, startSample, nSamples) {
  const spectrum = [];
  for (let hz = SCAN_LO; hz <= SCAN_HI; hz += SCAN_STEP) {
    spectrum.push({ hz, power: goertzel(samples, startSample, nSamples, hz) });
  }
  // Local maxima, strongest first, merged within MERGE_HZ.
  const maxima = spectrum.filter((p, i) =>
    (i === 0 || p.power >= spectrum[i - 1].power) &&
    (i === spectrum.length - 1 || p.power > spectrum[i + 1].power),
  ).sort((a, b) => b.power - a.power);
  const peaks = [];
  for (const m of maxima) {
    if (peaks.some((p) => Math.abs(p.hz - m.hz) < MERGE_HZ)) continue;
    peaks.push(m);
    if (peaks.length === TOP_PEAKS) break;
  }
  const sorted = spectrum.map((p) => p.power).sort((a, b) => a - b);
  const median = sorted[Math.floor(sorted.length / 2)] || 1e-12;
  return { peaks, median };
}

function midiOf(hz) {
  const midi = 69 + 12 * Math.log2(hz / 440);
  const rounded = Math.round(midi);
  const names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
  const name = names[((rounded % 12) + 12) % 12] + (Math.floor(rounded / 12) - 1);
  return { midiNote: name, cents: Math.round((midi - rounded) * 100) };
}

function ratesFor(hz) {
  const pick = (table) => {
    const rates = {};
    for (const [note, target] of Object.entries(table)) {
      const rate = target / hz;
      if (rate >= 0.5 && rate <= 2.0) rates[note] = Math.round(rate * 10000) / 10000;
    }
    return rates;
  };
  const rates = pick(TARGETS);
  return Object.keys(rates).length ? rates : pick(HIGH_TARGETS);
}

function analyze(file) {
  const samples = decode(file);
  const durationSec = samples.length / SR;
  const usable = durationSec - SKIP_SEC;
  const winSec = Math.min(WINDOW_SEC, Math.max(0.5, usable));
  // 1 window for short files, 2 for medium, 3 for long handshakes.
  const nWindows = usable <= winSec + 1 ? 1 : usable <= 3 * winSec ? 2 : 3;
  const offsets = [];
  for (let i = 0; i < nWindows; i++) {
    const span = usable - winSec;
    offsets.push(SKIP_SEC + (nWindows === 1 ? 0 : (span * i) / (nWindows - 1)));
  }

  const windows = [];
  let tonal = false;
  const allPeaks = []; // { hz, power, ratio, windowIndex }
  for (const atSec of offsets) {
    const start = Math.floor(atSec * SR);
    const n = Math.min(Math.floor(winSec * SR), samples.length - start);
    const { peaks, median } = scanWindow(samples, start, n);
    if (peaks[0] && peaks[0].power / median >= TONAL_RATIO) tonal = true;
    for (const p of peaks) {
      allPeaks.push({ ...p, ratio: p.power / median, windowIndex: windows.length });
    }
    windows.push({
      atSec: Math.round(atSec * 100) / 100,
      peaks: peaks.map((p) => ({ hz: p.hz, ...midiOf(p.hz) })),
    });
  }

  // Strongest *stable* peak: prefer peaks recurring (within MERGE_HZ) across
  // ≥2 windows, scored by summed prominence; fall back to the loudest single.
  let best = null;
  for (const p of allPeaks) {
    const kin = allPeaks.filter((q) => Math.abs(q.hz - p.hz) < MERGE_HZ);
    const windowsHit = new Set(kin.map((q) => q.windowIndex)).size;
    const score = kin.reduce((s, q) => s + q.ratio, 0) * (windowsHit > 1 ? 2 : 1);
    if (!best || score > best.score) best = { hz: p.hz, score, windowsHit };
  }

  return { durationSec: Math.round(durationSec * 100) / 100, tonal, windows, best };
}

const files = readdirSync(HERE).filter((f) => f.endsWith(".mp3")).sort();
const manifest = [];
for (const f of files) {
  const id = f.match(/^(\d+)-/)?.[1];
  process.stdout.write(`analyzing ${f} ... `);
  const { durationSec, tonal, windows, best } = analyze(resolve(HERE, f));
  const strongest = best ? { hz: best.hz, ...midiOf(best.hz) } : null;
  manifest.push({
    file: f,
    freesoundId: id ? Number(id) : null,
    durationSec,
    tags: TAGS[id] || ["mixed"],
    tonal,
    windows,
    strongestPeak: strongest,
    rates: best && tonal ? ratesFor(best.hz) : {},
  });
  console.log(
    `${durationSec}s ${tonal ? "tonal" : "noisy"}` +
    (strongest ? ` peak ${strongest.hz} Hz (${strongest.midiNote} ${strongest.cents >= 0 ? "+" : ""}${strongest.cents}¢)` : ""),
  );
}

const outPath = resolve(HERE, "manifest.json");
writeFileSync(outPath, JSON.stringify(manifest, null, 2) + "\n");
console.log(`\nwrote ${outPath} (${manifest.length} samples)`);
