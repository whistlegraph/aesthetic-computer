#!/usr/bin/env node
// play.mjs — pop play. quick "notes → instrument → fx → wav → open" from /pop.
//
// Usage:
//   node pop/bin/play.mjs --menu
//   node pop/bin/play.mjs --notes "C4*1 E4*1 G4*1 C5*2" --instrument supersaw --preset pad --open
//   node pop/bin/play.mjs --notes "A3*1 C4*1 E4*2" --instrument hoover --preset whoop --fx flange --open
//   node pop/bin/play.mjs --np pop/dance/trance-hook.np --instrument supersaw --bpm 138 --fx wobble:rate=2,depth=0.7 --fx bitcrush:bits=6 --open
//
// Note format (inline): "<NOTE>*<beats>" tokens, space-separated.
//   NOTE = C4 / C#4 / Bb3 / etc. (octave 4 = middle).  *beats default 1.
//   "r*1" = rest of 1 beat.
//
// .np: pop's notepat score format. Multi-syllable cells share their start
// time; this CLI extracts the first NOTE token per cell (one note per
// syllable), which is enough for instrumental playback.
//
// Defaults: 120 BPM, 48 kHz mono, normalized to −3 dB. WAV out to /tmp.
// --open hands the file to QuickTime Player (mp3/audio convention).

import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { spawn } from "node:child_process";
import { tmpdir } from "node:os";

import { MENU, loadInstrument, loadFx } from "../lib/menu.mjs";
import { resolveLfoHz } from "../dance/synths/skrill.mjs";

// ── arg parsing ────────────────────────────────────────────────────────
function parseArgs(argv) {
  const flags = { fx: [] };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--fx") { flags.fx.push(argv[++i]); continue; }
    if (a.startsWith("--")) {
      const key = a.slice(2);
      const next = argv[i + 1];
      if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
      else flags[key] = true;
    }
  }
  return flags;
}

// Split on `sep` at bracket depth 0 — so breakpoint lists `[t:v,t:v]`
// embedded as a value don't break the param-list comma.
function splitTopLevel(s, sep) {
  const out = [];
  let depth = 0, cur = "";
  for (const ch of s) {
    if (ch === "[") depth++;
    else if (ch === "]") depth--;
    if (ch === sep && depth === 0) { out.push(cur); cur = ""; }
    else cur += ch;
  }
  if (cur !== "") out.push(cur);
  return out;
}

// fx spec: "wobble" · "wobble:rate=2,depth=0.7" · "wobble:rate=1/8,depth=[0:0,2:0.7,4:0]"
//   "1/N", "1/Nt" (triplet), "1/Nd" (dotted) → Hz (needs --bpm)
//   "[t0:v0,t1:v1,...]" → breakpoint env: [{time:t0, [k]:v0}, ...]
function parseFxSpec(spec, bpm) {
  // Only split on the FIRST colon — breakpoint envs use `:` as time:value sep.
  const i = spec.indexOf(":");
  const name = i < 0 ? spec : spec.slice(0, i);
  const paramStr = i < 0 ? "" : spec.slice(i + 1);
  const opts = {};
  if (!paramStr) return { name, opts };
  for (const kv of splitTopLevel(paramStr, ",")) {
    const eq = kv.indexOf("=");
    if (eq < 0) continue;
    const k = kv.slice(0, eq).trim();
    const v = kv.slice(eq + 1).trim();
    if (v.startsWith("[") && v.endsWith("]")) {
      // breakpoint env: [t0:v0,t1:v1,...]
      const inner = v.slice(1, -1);
      const points = [];
      for (const pair of splitTopLevel(inner, ",")) {
        const [tStr, vStr] = pair.split(":");
        const time = Number(tStr);
        const val  = Number(vStr);
        if (Number.isFinite(time) && Number.isFinite(val)) {
          points.push({ time, [k]: val });
        }
      }
      opts[k] = points;
    } else if (/^1\/\d+(\.\d+)?[td]?$/.test(v)) {
      opts[k] = resolveLfoHz(v, bpm);
    } else {
      const num = Number(v);
      opts[k] = Number.isFinite(num) ? num : v;
    }
  }
  return { name, opts };
}

// ── notes → events ────────────────────────────────────────────────────
const STEP = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };

function noteNameToMidi(name) {
  if (name === "r" || name === "R") return null; // rest
  const m = name.match(/^([A-Ga-g])([#b]?)(-?\d+)$/);
  if (!m) return NaN;
  let n = STEP[m[1].toUpperCase()];
  if (m[2] === "#") n += 1;
  else if (m[2] === "b") n -= 1;
  return n + (parseInt(m[3], 10) + 1) * 12;
}

function parseInlineNotes(s, bpm) {
  const beatSec = 60 / bpm;
  const events = [];
  let t = 0;
  for (const tok of s.trim().split(/\s+/)) {
    const [n, b] = tok.split("*");
    const beats = b ? parseFloat(b) : 1;
    const midi = noteNameToMidi(n);
    if (midi === null) { t += beats * beatSec; continue; }
    if (!Number.isFinite(midi)) continue;
    events.push({ startSec: t, midi, durSec: beats * beatSec * 0.95, gain: 0.9 });
    t += beats * beatSec;
  }
  return { events, totalSec: t };
}

// .np: `NOTE:syllable*beats` cells, space-separated, advancing time.
// Lines starting with `#`, `verse`, or blank are skipped. We take one
// note per cell (the first one) — enough for instrumental preview.
function parseNpFile(path, bpm) {
  const beatSec = 60 / bpm;
  const raw = readFileSync(path, "utf8");
  const events = [];
  let t = 0;
  for (const line of raw.split("\n")) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    if (trimmed.startsWith("#")) continue;
    if (/^verse\b/i.test(trimmed)) continue;
    for (const tok of trimmed.split(/\s+/)) {
      const m = tok.match(/^([A-G][#b]?-?\d+):[^*\s]*\*([\d.]+)$/);
      if (!m) continue;
      const midi = noteNameToMidi(m[1]);
      const beats = parseFloat(m[2]);
      if (!Number.isFinite(midi) || !Number.isFinite(beats)) continue;
      events.push({ startSec: t, midi, durSec: beats * beatSec * 0.95, gain: 0.9 });
      t += beats * beatSec;
    }
  }
  return { events, totalSec: t };
}

// ── menu print ────────────────────────────────────────────────────────
function printMenu() {
  console.log("\npop menu — pickable building blocks across /pop\n");
  for (const [section, items] of Object.entries(MENU)) {
    console.log(`${section}:`);
    if (Array.isArray(items)) { console.log(`  ${items.join(", ")}`); console.log(""); continue; }
    if (section === "scales") {
      for (const [k, v] of Object.entries(items)) console.log(`  ${k.padEnd(14)} [${v.join(", ")}]`);
      console.log(""); continue;
    }
    if (section === "percussion") {
      console.log(`  voices: ${items.voices.join(", ")}`);
      console.log(`  ${items.note}`);
      console.log(""); continue;
    }
    for (const [name, def] of Object.entries(items)) {
      const presets = def.presets ? ` (${def.presets.join(", ")})` : "";
      const lane = def.lane ? ` · ${def.lane}` : "";
      const blurb = def.blurb || "";
      const inlineMark = def.inline && !def.file ? "  [inline]" : "";
      console.log(`  ${(name + presets).padEnd(32)}${lane.padEnd(18)} — ${blurb}${inlineMark}`);
    }
    console.log("");
  }
  console.log("usage:");
  console.log("  play.mjs --notes \"C4*1 E4*1 G4*2\" --instrument supersaw --preset pad --fx wobble --open");
  console.log("  play.mjs --np path/to/score.np --instrument hoover --bpm 138 --fx flange --open");
}

// ── WAV writer (mono, 16-bit PCM) ─────────────────────────────────────
function writeWavMono16(path, float32, sampleRate) {
  const n = float32.length;
  const bytes = n * 2;
  const header = Buffer.alloc(44);
  header.write("RIFF", 0);
  header.writeUInt32LE(36 + bytes, 4);
  header.write("WAVE", 8);
  header.write("fmt ", 12);
  header.writeUInt32LE(16, 16);
  header.writeUInt16LE(1, 20);          // PCM
  header.writeUInt16LE(1, 22);          // channels
  header.writeUInt32LE(sampleRate, 24);
  header.writeUInt32LE(sampleRate * 2, 28);
  header.writeUInt16LE(2, 32);          // block align
  header.writeUInt16LE(16, 34);
  header.write("data", 36);
  header.writeUInt32LE(bytes, 40);
  const pcm = Buffer.alloc(bytes);
  for (let i = 0; i < n; i++) {
    let s = float32[i];
    if (s > 1) s = 1; else if (s < -1) s = -1;
    pcm.writeInt16LE((s * 32767) | 0, i * 2);
  }
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, Buffer.concat([header, pcm]));
}

// ── main ──────────────────────────────────────────────────────────────
async function main() {
  const flags = parseArgs(process.argv.slice(2));

  if (flags.menu || flags.help) { printMenu(); return; }

  const SAMPLE_RATE = Number(flags.sr ?? 48000);
  const bpm = Number(flags.bpm ?? 120);

  let parsed;
  if (flags.np) parsed = parseNpFile(resolve(process.cwd(), flags.np), bpm);
  else if (flags.notes) parsed = parseInlineNotes(flags.notes, bpm);
  else {
    console.error("provide --notes \"C4*1 E4*1 G4*2\" or --np path/to/score.np  (or --menu)");
    process.exit(1);
  }

  if (parsed.events.length === 0) { console.error("no notes parsed"); process.exit(1); }

  const instName = flags.instrument || flags.inst || "supersaw";
  const inst = MENU.instruments[instName];
  if (!inst) { console.error(`unknown instrument "${instName}". try --menu`); process.exit(1); }

  const mixEvent = await loadInstrument(instName);

  const tailSec = Number(flags.tail ?? 1.8);
  const totalSamples = Math.ceil((parsed.totalSec + tailSec) * SAMPLE_RATE);
  const out = new Float32Array(totalSamples);

  const instOpts = { sampleRate: SAMPLE_RATE };
  if (flags.preset) instOpts.preset = flags.preset;
  if (flags.detune !== undefined) instOpts.detuneCents = Number(flags.detune);
  if (flags.voices !== undefined) instOpts.voices = Number(flags.voices);

  const presetTag = flags.preset ? `/${flags.preset}` : "";
  console.log(`→ ${instName}${presetTag} · bpm=${bpm} · ${parsed.events.length} notes · ${(parsed.totalSec + tailSec).toFixed(2)}s`);
  for (const ev of parsed.events) mixEvent(ev, out, instOpts);

  for (const spec of flags.fx) {
    const { name, opts } = parseFxSpec(spec, bpm);
    let apply;
    try { apply = await loadFx(name); } catch (e) { console.error(`✗ ${e.message}`); continue; }
    const tag = Object.keys(opts).length ? " " + JSON.stringify(opts) : "";
    console.log(`  fx · ${name}${tag}`);
    apply(out, { sampleRate: SAMPLE_RATE, ...opts });
  }

  // peak-normalize to −3 dB
  let peak = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
  if (peak > 0) {
    const norm = 0.707 / peak;
    for (let i = 0; i < out.length; i++) out[i] *= norm;
  }

  const stamp = new Date().toISOString().replace(/[:.]/g, "-").slice(0, 19);
  const outPath = flags.out
    ? resolve(process.cwd(), flags.out)
    : resolve(tmpdir(), `pop-${instName}${flags.preset ? "-" + flags.preset : ""}-${stamp}.wav`);

  writeWavMono16(outPath, out, SAMPLE_RATE);
  console.log(`✓ ${outPath}`);

  if (flags.open) {
    spawn("open", ["-a", "QuickTime Player", outPath], { stdio: "ignore", detached: true }).unref();
  }
}

main().catch((e) => { console.error(e); process.exit(1); });
