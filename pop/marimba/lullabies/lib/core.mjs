// core.mjs — the shared lullaby render core for the marimbaba-riff lane.
//
// Reuses the real AC marimba synth (pop/marimba/synths/marimba.mjs —
// modal mallet model, the same one behind marimbaba.mp3) so every
// variation sounds like the instrument, not a re-synth. A variation just
// builds an `events` array and calls renderLullaby(); this module does the
// stereo mix (per-event equal-power pan), a soft Schroeder reverb, peak
// normalize + gentle fades, a 16-bit WAV, and a tender ffmpeg mp3 master.
//
// Event shape: { preset, startSec, midi, durSec, gain, decayMul?, pan? }

import { renderMarimba, MARIMBA_PRESETS } from "../../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { spawnSync } from "node:child_process";
import { homedir, tmpdir } from "node:os";

export const SR = 48_000;
export { MARIMBA_PRESETS };

// ── note-name → MIDI (C4 = 60). Accepts "C5", "Bb4", "F#3", "A4". ──────────
const SEMI = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
export function m(name) {
  if (typeof name === "number") return name;
  const mt = /^([A-Ga-g])([#b]?)(-?\d+)$/.exec(name.trim());
  if (!mt) throw new Error(`bad note: ${name}`);
  let s = SEMI[mt[1].toUpperCase()];
  if (mt[2] === "#") s += 1; else if (mt[2] === "b") s -= 1;
  return (parseInt(mt[3], 10) + 1) * 12 + s;
}

// ── soft Schroeder reverb (4 combs + 2 allpass), stereo, dreamy + glassy ──
function reverb(L, R, { wet = 0.28, decay = 0.82, damp = 0.4 } = {}) {
  const n = L.length;
  const cds = [0.0297, 0.0371, 0.0411, 0.0437];
  const CD = cds.map((d) => Math.floor(d * SR));
  const cbL = CD.map((d) => new Float32Array(d));
  const cbR = CD.map((d) => new Float32Array(d));
  const ci = CD.map(() => 0);
  const lpL = CD.map(() => 0), lpR = CD.map(() => 0);
  const AD = [Math.floor(0.005 * SR), Math.floor(0.0017 * SR)];
  const abL = AD.map((d) => new Float32Array(d));
  const abR = AD.map((d) => new Float32Array(d));
  const ai = AD.map(() => 0);
  const apFb = 0.5;
  for (let i = 0; i < n; i++) {
    const inL = L[i], inR = R[i];
    let cL = 0, cR = 0;
    for (let k = 0; k < CD.length; k++) {
      const dL = cbL[k][ci[k]], dR = cbR[k][ci[k]];
      cL += dL; cR += dR;
      lpL[k] = dL * (1 - damp) + lpL[k] * damp;
      lpR[k] = dR * (1 - damp) + lpR[k] * damp;
      cbL[k][ci[k]] = inL + lpL[k] * decay;
      cbR[k][ci[k]] = inR + lpR[k] * decay;
      ci[k] = (ci[k] + 1) % CD[k];
    }
    cL /= CD.length; cR /= CD.length;
    for (let k = 0; k < AD.length; k++) {
      const dL = abL[k][ai[k]], dR = abR[k][ai[k]];
      const oL = -apFb * cL + dL, oR = -apFb * cR + dR;
      abL[k][ai[k]] = cL + apFb * oL;
      abR[k][ai[k]] = cR + apFb * oR;
      ai[k] = (ai[k] + 1) % AD[k];
      cL = oL; cR = oR;
    }
    L[i] += cL * wet; R[i] += cR * wet;
  }
}

// healing bed — a constant pure-sine drone under the whole piece at a
// "healing" frequency (default 528 Hz, the Solfeggio MI / "repair" tone)
// plus its sub-octave for body, with a very slow amplitude breath and
// long self-fades. Quiet enough to be felt, not to fight the melody.
// Disable with opts.healing = false; retune with opts.healingHz.
function healingBed(L, R, { hz = 528, gain = 0.05 } = {}) {
  const n = L.length;
  const dp1 = TAU_ * hz / SR, dp2 = TAU_ * (hz / 2) / SR, dlfo = TAU_ * 0.07 / SR;
  let p1 = 0, p2 = 0, pl = 0;
  const fade = Math.floor(2.5 * SR);
  for (let i = 0; i < n; i++) {
    p1 += dp1; p2 += dp2; pl += dlfo;
    let env = 0.78 + 0.22 * Math.sin(pl);
    if (i < fade) env *= i / fade;
    if (i > n - fade) env *= (n - i) / fade;
    const v = (Math.sin(p1) * 0.55 + Math.sin(p2) * 0.45) * gain * env;
    L[i] += v; R[i] += v;
  }
}
const TAU_ = Math.PI * 2;

function writeWav16(path, L, R) {
  const n = L.length, bytes = n * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 4, 28);
  buf.writeUInt16LE(4, 32); buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(bytes, 40);
  let o = 44;
  for (let i = 0; i < n; i++) {
    const l = Math.max(-1, Math.min(1, L[i])), r = Math.max(-1, Math.min(1, R[i]));
    buf.writeInt16LE((l < 0 ? l * 32768 : l * 32767) | 0, o); o += 2;
    buf.writeInt16LE((r < 0 ? r * 32768 : r * 32767) | 0, o); o += 2;
  }
  writeFileSync(path, buf);
}

const expand = (p) => (p && p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);

// the gentle lullaby master: warm, quiet, lots of room. Soft top, low
// loudness target so it breathes; a tender limiter, no aggressive comp.
const MASTER = [
  "highpass=f=30",
  "equalizer=f=180:t=q:w=1.1:g=1.0",     // a little warmth
  "equalizer=f=320:t=q:w=1.2:g=-1.0",    // de-mud
  "equalizer=f=9000:t=q:w=0.9:g=-1.4",   // soften the mallet edge
  "treble=g=-1.0:f=11000",
  "lowpass=f=15500",
  "acompressor=threshold=-22dB:ratio=1.8:attack=40:release=320:makeup=1.6:knee=8",
  "loudnorm=I=-17:TP=-1.5:LRA=12",
  "alimiter=limit=0.95:attack=12:release=160",
].join(",");

// renderLullaby(events, opts) — mix, reverb, normalize, fade, master to mp3.
// opts: { name, out?, tailSec?, reverb?, peak?, fadeIn?, fadeOut?, title? }
export function renderLullaby(events, opts = {}) {
  const { name = "lullaby", tailSec = 4.0, peak = 0.86 } = opts;
  const HERE = opts.here ?? process.cwd();
  let end = 0;
  for (const e of events) end = Math.max(end, (e.startSec ?? 0) + (e.durSec ?? 0));
  const N = Math.ceil((end + tailSec) * SR);
  const L = new Float32Array(N), R = new Float32Array(N);

  for (const e of events) {
    if (!Number.isFinite(e.midi) || !(e.durSec > 0) || (e.gain ?? 0) === 0) continue;
    const seg = renderMarimba(e, { sampleRate: SR });
    const s0 = Math.floor((e.startSec ?? 0) * SR);
    const pan = e.pan ?? 0;
    const ang = (pan * 0.5 + 0.5) * (Math.PI / 2);
    const gL = Math.cos(ang), gR = Math.sin(ang);
    for (let i = 0; i < seg.length; i++) {
      const d = s0 + i; if (d < 0 || d >= N) continue;
      L[d] += seg[i] * gL; R[d] += seg[i] * gR;
    }
  }

  if (opts.healing !== false) {
    healingBed(L, R, { hz: opts.healingHz ?? 528, gain: opts.healingGain ?? 0.05 });
  }
  reverb(L, R, opts.reverb ?? {});

  let pk = 0;
  for (let i = 0; i < N; i++) { const a = Math.max(Math.abs(L[i]), Math.abs(R[i])); if (a > pk) pk = a; }
  if (pk > 0) { const g = peak / pk; for (let i = 0; i < N; i++) { L[i] *= g; R[i] *= g; } }
  const fin = Math.floor((opts.fadeIn ?? 0.8) * SR), fout = Math.floor((opts.fadeOut ?? 3.5) * SR);
  for (let i = 0; i < fin && i < N; i++) { const g = 0.5 - 0.5 * Math.cos(Math.PI * i / fin); L[i] *= g; R[i] *= g; }
  for (let i = 0; i < fout && i < N; i++) { const g = 0.5 - 0.5 * Math.cos(Math.PI * i / fout); const x = N - 1 - i; L[x] *= g; R[x] *= g; }

  const outMp3 = resolve(expand(opts.out) ?? resolve(HERE, "..", "out", `${name}.mp3`));
  mkdirSync(dirname(outMp3), { recursive: true });
  // scratch WAV lives in the OS tmpdir (never beside the mp3) and is
  // deleted after mastering, so out/ only ever holds finished .mp3s.
  const rawWav = join(tmpdir(), `juke-${name}-${process.pid}-raw.wav`);
  writeWav16(rawWav, L, R);
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error", "-i", rawWav, "-af", MASTER,
    "-c:a", "libmp3lame", "-b:a", "320k",
    "-metadata", `title=${opts.title ?? name}`, "-metadata", "artist=jeffrey", "-metadata", "album=lullabies",
    outMp3,
  ], { stdio: "inherit" });
  try { unlinkSync(rawWav); } catch {}
  if (r.status !== 0) throw new Error("ffmpeg master failed");
  return { mp3: outMp3, durationSec: N / SR };
}
