#!/usr/bin/env node
// vinyl.mjs — vinyl crackle + tape hiss bed for the booch lane.
// Pure-float per-sample DSP (C-portable). Three layers:
//   1. pink-ish noise base (LP-filtered white noise) — the constant tape hiss
//   2. low-rate impulse train — the random pops/ticks from the groove
//   3. occasional bigger "scrape" impulses with a short exponential tail —
//      the surface dust catching the stylus
//
// Library:
//   import { mixVinyl, renderVinyl, VINYL_PRESETS } from "...";
//   mixVinyl(out, startSec, durSec, opts)        // node bed render path
//   renderVinyl(durSec, opts) -> Float32Array    // bare DSP engine
//
// CLI demo:
//   node pop/booch/synths/vinyl.mjs                       # 6s dusty preset
//   node pop/booch/synths/vinyl.mjs --preset hiss --dur 4 --out ~/v.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const DEFAULT_SAMPLE_RATE = 48_000;
const DEFAULT_PRESET = "dusty";

// ── presets ────────────────────────────────────────────────────────────
// hissGain      : steady-state pink/LP noise level
// hissCutoff    : LP cutoff for the noise bed (Hz)
// tickRate      : average ticks/sec
// tickGain      : per-tick amplitude (uniform 0..tickGain)
// scrapeRate    : average bigger scrapes/sec
// scrapeGain    : amplitude scale for scrapes
// scrapeDecay   : exp decay seconds for scrape tail
export const VINYL_PRESETS = {
  // Classic dusty SP-1200 surface — present but not loud, ticks 3/sec.
  dusty: {
    hissGain: 0.05, hissCutoff: 3500,
    tickRate: 3.0, tickGain: 0.12,
    scrapeRate: 0.25, scrapeGain: 0.14, scrapeDecay: 0.04,
  },
  // Clean — barely audible, just a wisp of warmth.
  clean: {
    hissGain: 0.025, hissCutoff: 5000,
    tickRate: 0.6, tickGain: 0.06,
    scrapeRate: 0.05, scrapeGain: 0.07, scrapeDecay: 0.03,
  },
  // Hiss-forward — old cassette feel, more bed noise, fewer pops.
  hiss: {
    hissGain: 0.10, hissCutoff: 4200,
    tickRate: 1.2, tickGain: 0.08,
    scrapeRate: 0.10, scrapeGain: 0.10, scrapeDecay: 0.05,
  },
};

// ── helpers ────────────────────────────────────────────────────────────
function makeRng(seedStr) {
  let s = 2166136261 >>> 0;
  for (let i = 0; i < seedStr.length; i++) {
    s ^= seedStr.charCodeAt(i);
    s = Math.imul(s, 16777619);
  }
  s = s >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

// ── the DSP engine ─────────────────────────────────────────────────────
export function renderVinyl(durSec, opts = {}) {
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const presetName = opts.preset || DEFAULT_PRESET;
  const P = { ...(VINYL_PRESETS[presetName] || VINYL_PRESETS[DEFAULT_PRESET]), ...opts.params };
  const gain = Number.isFinite(opts.gain) ? opts.gain : 1.0;
  if (gain === 0 || !Number.isFinite(durSec) || durSec <= 0) return new Float32Array(0);

  const ns = Math.ceil(durSec * sampleRate);
  const out = new Float32Array(ns);
  const rng = makeRng(opts.seed ?? `vinyl:${presetName}:${durSec.toFixed(3)}`);

  // simple 1-pole LP for noise bed
  const fc = Math.min(Math.max(80, P.hissCutoff), sampleRate / 3);
  const a = 1 - Math.exp(-2 * Math.PI * fc / sampleRate);
  let lp = 0;

  // pre-roll the tick + scrape decisions deterministically (per-sample
  // probability so the rate scales with the preset)
  const tickProb = (P.tickRate ?? 3.0) / sampleRate;
  const scrapeProb = (P.scrapeRate ?? 0.25) / sampleRate;
  const scrapeDecS = Math.max(1, Math.floor((P.scrapeDecay ?? 0.04) * sampleRate));

  // active scrapes — start sample + amplitude
  const scrapes = [];

  for (let i = 0; i < ns; i++) {
    // pink-ish: LP-filter white noise toward the cutoff
    const w = rng() * 2 - 1;
    lp += a * (w - lp);
    let s = lp * (P.hissGain ?? 0.05);

    // ticks — tiny, narrow, ±polarity
    if (rng() < tickProb) {
      s += (rng() * 2 - 1) * (P.tickGain ?? 0.12);
    }

    // schedule a scrape
    if (rng() < scrapeProb) {
      scrapes.push({ start: i, amp: (rng() * 0.6 + 0.4) * (P.scrapeGain ?? 0.14) * (rng() < 0.5 ? -1 : 1) });
    }
    // mix active scrapes (exponential decay)
    for (let k = scrapes.length - 1; k >= 0; k--) {
      const sc = scrapes[k];
      const dt = i - sc.start;
      if (dt >= scrapeDecS) { scrapes.splice(k, 1); continue; }
      const env = Math.exp(-dt / (scrapeDecS / 4));
      s += sc.amp * env * (rng() * 2 - 1) * 0.6;       // crackly, noisy tail
    }

    out[i] = s * gain;
  }
  return out;
}

// ── node-side buffer mixer ─────────────────────────────────────────────
export function mixVinyl(out, startSec, durSec, opts = {}) {
  if (!(out instanceof Float32Array)) return;
  const sampleRate = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const seg = renderVinyl(durSec, { ...opts, sampleRate });
  const startIdx = Math.floor(startSec * sampleRate);
  for (let i = 0; i < seg.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    out[dst] += seg[i];
  }
}

// ── CLI demo ───────────────────────────────────────────────────────────
const isMain =
  process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
  const argv = process.argv.slice(2);
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) {
      const key = a.slice(2);
      const next = argv[i + 1];
      if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
      else flags[key] = true;
    }
  }
  function expandHome(p) {
    if (!p) return p;
    if (p === "~") return homedir();
    if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
    return p;
  }
  const HERE = dirname(fileURLToPath(import.meta.url));
  const SR = 48_000;
  const preset = flags.preset || DEFAULT_PRESET;
  const dur = Number(flags.dur ?? 6);
  const outPath = expandHome(flags.out) || resolve(HERE, `vinyl-${preset}.mp3`);

  console.log(`→ vinyl demo · preset=${preset} · ${dur}s`);
  const out = renderVinyl(dur, { sampleRate: SR, preset });
  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) { const n = 0.7 / peak; for (let i = 0; i < out.length; i++) out[i] *= n; }
  mkdirSync(dirname(outPath), { recursive: true });
  const rawPath = `${outPath}.f32.raw`;
  const buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
  writeFileSync(rawPath, buf);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "3", outPath,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outPath}`);
}
