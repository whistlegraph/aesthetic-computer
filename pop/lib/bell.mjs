// bell.mjs — the physically-modeled bell as a /pop voice.
//
// Wraps the validated C engine at pop/bell/c/bell (FEM shell modal synthesis)
// so any track renderer can drop bell strikes into a mix. Returns plain
// Float32Array buffers at 48 kHz — the /pop convention. The engine is built
// on first use if missing or stale.
//
//   import { renderBell, renderBellMono, bellSampleBank } from "../lib/bell.mjs";
//   const { L, R } = renderBell({ note: "A4", material: "bronze", dur: 6 });
//   // ...mix L/R into your stereo bus at the strike time.
//
// Materials:  bronze brass steel aluminum silver glass gold   (pitch + decay)
// Geometries: church handbell tubular bowl glass               (timbre)
//
// Pitch is set by `note` (name like "C#5", or a bare Hz number via `freq`);
// `material`/`geometry` shape the timbre and ring; `vel` (0..1) the strike.

import { spawnSync } from "node:child_process";
import { existsSync, statSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "..", "bell", "c", "bell");
const CSRC = resolve(HERE, "..", "bell", "c", "bell.c");
const BUILD = resolve(HERE, "..", "bell", "c", "build.sh");

export const BELL_MATERIALS = [
  "bronze", "brass", "steel", "aluminum", "silver", "glass", "gold",
];
export const BELL_GEOMETRIES = ["church", "handbell", "tubular", "bowl", "glass"];
export const BELL_SR = 48_000;

// Build the engine if it is missing or older than its source.
function ensureEngine() {
  const stale =
    !existsSync(ENGINE) ||
    (existsSync(CSRC) && statSync(CSRC).mtimeMs > statSync(ENGINE).mtimeMs);
  if (stale) {
    const b = spawnSync("bash", [BUILD], { stdio: "inherit" });
    if (b.status !== 0) throw new Error("bell: engine build failed");
  }
}

// Read a stereo f32 WAV into { L, R, sampleRate }.
function readWavStereoF32(path) {
  const buf = readFileSync(path);
  let ch = 2, sr = BELL_SR, bits = 32, fmt = 3, i = 12;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4);
    const size = buf.readUInt32LE(i + 4);
    const body = i + 8;
    if (id === "fmt ") {
      fmt = buf.readUInt16LE(body);
      ch = buf.readUInt16LE(body + 2);
      sr = buf.readUInt32LE(body + 4);
      bits = buf.readUInt16LE(body + 14);
    } else if (id === "data") {
      const bps = bits / 8;
      const frames = Math.floor(size / (bps * ch));
      const L = new Float32Array(frames);
      const R = new Float32Array(frames);
      for (let f = 0; f < frames; f++) {
        const o = body + f * ch * bps;
        const rd = (k) =>
          fmt === 3 ? buf.readFloatLE(o + k * bps) : buf.readInt16LE(o + k * bps) / 32768;
        L[f] = rd(0);
        R[f] = ch > 1 ? rd(1) : L[f];
      }
      return { L, R, sampleRate: sr };
    }
    i = body + size + (size & 1);
  }
  throw new Error(`bell: no data chunk in ${path}`);
}

function engineArgs({ note, freq, material, geometry, dur, vel, sr, maxm }) {
  const pitch = freq != null ? String(freq) : note ?? "A4";
  const a = ["--note", pitch];
  if (material) a.push("--material", material);
  if (geometry) a.push("--geometry", geometry);
  if (dur != null) a.push("--dur", String(dur));
  if (vel != null) a.push("--vel", String(vel));
  if (sr != null) a.push("--sr", String(sr));
  if (maxm != null) a.push("--maxm", String(maxm));
  return a;
}

// Render one strike -> { L, R, sampleRate } stereo Float32Arrays.
export function renderBell(opts = {}) {
  ensureEngine();
  const tmp = mkdtempSync(join(tmpdir(), "bell-"));
  const wav = join(tmp, "out.wav");
  try {
    const r = spawnSync(ENGINE, [...engineArgs(opts), "--out", wav], {
      stdio: ["ignore", "ignore", "inherit"],
    });
    if (r.status !== 0) throw new Error("bell: render failed");
    return readWavStereoF32(wav);
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// Convenience mono downmix (many track buses are mono per voice).
export function renderBellMono(opts = {}) {
  const { L, R, sampleRate } = renderBell(opts);
  const m = new Float32Array(L.length);
  for (let i = 0; i < L.length; i++) m[i] = 0.5 * (L[i] + R[i]);
  return { samples: m, sampleRate };
}

// Solve + return the modes (and geometry/material) without rendering audio —
// useful for inspection or feeding pop/bell/bin/viz.mjs.
export function bellModes(opts = {}) {
  ensureEngine();
  const tmp = mkdtempSync(join(tmpdir(), "bell-"));
  const json = join(tmp, "modes.json");
  try {
    const r = spawnSync(ENGINE, [...engineArgs(opts), "--modes", json], {
      stdio: ["ignore", "ignore", "inherit"],
    });
    if (r.status !== 0) throw new Error("bell: solve failed");
    return JSON.parse(readFileSync(json, "utf8"));
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }
}

// Render a bank of pitched one-shots to WAV files (for sample-playback
// pipelines). `notes` is an array of note names or Hz. Returns the paths.
export function bellSampleBank({ notes, material = "bronze", geometry = "church", dur = 6, dir }) {
  ensureEngine();
  if (!dir) throw new Error("bell: bellSampleBank needs a `dir`");
  const paths = [];
  for (const n of notes) {
    const safe = String(n).replace(/[^a-zA-Z0-9.+-]/g, "_");
    const out = join(dir, `bell-${geometry}-${material}-${safe}.wav`);
    const r = spawnSync(
      ENGINE,
      [...engineArgs({ note: n, material, geometry, dur }), "--out", out],
      { stdio: ["ignore", "ignore", "inherit"] },
    );
    if (r.status !== 0) throw new Error(`bell: bank render failed for ${n}`);
    paths.push(out);
  }
  return paths;
}
