#!/usr/bin/env node
// render9.mjs — "whistlegraph cult --- remix (v10, kicks first)"
//
// v9 = v3 × v5. @jeffrey, listening back: "i miss v3 now" — "i like the
// more steady vocal loop" — "can we merge them for a v9". So this is v5's
// record (the ten acts, the signal layer, the tube, the wiggle, the other
// people's cults) carrying v3's spine: the four-line chorus as a STEADY
// LOOP, cold-opened at bar 0 and restated roughly every eight bars all the
// way through, no two statements alike (v3's CHORUS_TAKES grammar). The
// narrative stops withholding the message; instead the acts orchestrate
// AROUND a message that never stops looping — and the one place the loop
// breaks (act IV, the turn) is now the loudest thing in the record.
//
// v10.1, @jeffrey again, four asks in one breath: "remove 'i wanna hide
// away' and 'i wanna run real fast' until the end" · "in the opening we
// should have lots of the 'dot dot dot'" · "from all the videos — samples
// and aesthetivoxed" · "to blend in with the phone tones — and any way we
// can find hang up sounds, phone hang up click and other standard sounds,
// as samples?" So: the loop still never stops, but its two WORDED lines
// are withheld until act VII — before that every statement is morse-only,
// dots standing where the words will land (wordsIn() is the gate). The
// shipped opening carries the dots from all nine posts — after a second
// pass ("too chaotic … wayyyy less … all extended a lot … more in the
// background"), as ONE granular-stretched dot at a time drifting behind
// the dial, each video appearing exactly once. And the signal layer grows a REAL
// phone alongside its synthesized one — CC0 freesound gestures in phone/
// (bin/phones.sh rebuilds; manifest tracked): a receiver picked up under
// the first kick and again at the turn, a rotary dial in the switchboard,
// and the record now ends on an actual handset meeting an actual cradle,
// with a distant busy signal after it.
//
// Everything below this paragraph that talks about v5 still applies to the
// bed, the busses and the acts; only the vocal plan changed.
//
// v4 was a good sequence of sections. @jeffrey heard it and asked for five
// things, the last of which reframes the other four:
//
//   1. "when the dashes cross over we could definitely pitch wiggle with them"
//   2. "and pumping them with more tubular vibes"
//   3. "and want a thicker kick / POW / like a nice electro synth keek"
//   4. "and more phone beeps and bops / and clicks and taps"
//   5. "lets add an overall narrative to the musical composition now"
//
// …then a sixth, mid-build:
//
//   6. "menu band's trackdrum sliding sound could be GREAT to add — some
//       skidding and sliding in the perc"
//
// ══ THE NARRATIVE ══════════════════════════════════════════════════════
//
// The source is eight seconds of three friends chanting morse at a camera
// with the caption "dot dot dot dash dash dash dot dot dot (jk)" — which is
// SOS, disclaimed. So the story the material is already telling is:
//
//     A SIGNAL GOES OUT, AND IT IS ANSWERED, AND THE JOKE TURNS OUT
//     TO BE TRUE.
//
// Nine acts (v10.1 — THE HUMANS is cut), 112 bars, 3:44 full / 3:28
// shipped. Nothing in the track exists that does not belong to one of them.
//
//   I    CARRIER      0:00  A channel opens before anything is said. DTMF
//                          tones dial C-U-L-T (keypad 2-8-5-8) into empty
//                          air over a drone. No drums, no words.
//   II   THREE VOICES 0:16  The pulse arrives and the three registers enter
//                          ONE AT A TIME — Jeffrey B2, then Alex F#3, then
//                          Camille C#4 — so you learn there are three people
//                          before you learn what they are saying. At bar 14
//                          they land on the same syllable for the first
//                          time, out of tune with each other, and wiggle
//                          into unison. Then SOS, twice.
//   III  THE MESSAGE  0:48  The hook, legible: dash · i wanna · dash ·
//                          i wanna · run REAL fast · dot dot. The thick kick
//                          lands. The dashes ring in a tube.
//   IV   THE SECRET   1:20  THE TURN. Kick out. The beeps stop — nobody is
//                          transmitting now — and one lone click is the
//                          receiver being picked up. The word arrives on its
//                          own, harmonised: CULT.
//   V    THE REPLY    1:36  Something answers. The hook returns, and in the
//                          hole it leaves, SOS comes back — beeped, in DTMF,
//                          from the far side of the stereo field.
//   VI   IT SPREADS   2:08  No hook. Pure harmonic travel, a switchboard of
//                          beeps, and the held dashes at full wiggle: the
//                          three voices have become many and they no longer
//                          agree about pitch.
//   VII  THE WHOLE    2:32  The fullest. The complete four-line chorus that
//        MESSAGE            the record has been withholding — "run real fast
//                          / i wanna hide away / i wanna dash / dot dot dash"
//                          — over the home progression, which the harmony
//                          also arrives at here for the first time.
//   VIII RECOGNITION  3:12  Elements peel off, one per bar.
//   IX   CARRIER OFF  3:28  The drone walks out. The beeps outlast it, and
//                          the last sound on the record is a hang-up click,
//                          after the music has already stopped. (v10.1:
//                          THE HUMANS — the unprocessed original take —
//                          is cut; no vocal on the record skips the
//                          aesthetivox anymore.)
//
// ══ THE FOUR SOUND-DESIGN ASKS ═════════════════════════════════════════
//
// (1) PITCH WIGGLE AT THE CROSSOVER. dashStack() puts three real people on
//     one syllable 28 ms apart; drift/spread puts two held dashes on top of
//     each other. Those are the crossovers, and only there does `wiggle`
//     turn on: a per-performer vibrato rate/phase plus a slow detune drift,
//     with the depth RAMPED IN over the first third of the note so the
//     attack stays in tune and only the sustain shimmers. Depth also grows
//     across the record — 4 cents when the three of them first meet, 22 by
//     the time it spreads — because the narrative is a unison coming apart.
//
// (2) TUBULAR PUMP. The dashes get their own bus. It runs through a bank of
//     four tuned feedback combs (B2 · B3 · F#4 · D5, damped at 2.4 kHz),
//     which is literally a resonant tube, then a low-drive asymmetric tube
//     saturation with a DC block, then a DEEP pump keyed off kick AND snare
//     — depth 0.72 against the bed's 0.50. Hollow and ringing, not fizzy:
//     the combs are scaled by (1-fb) so they resonate at unity, and the
//     saturation drive is 1.35, which is colour rather than distortion.
//
// (3) A THICK ELECTRO KICK. v4's kick was deliberately soft ("reads as a
//     pulse rather than a punch"). v5 goes the other way: a deeper and
//     faster pitch sweep (200 Hz → 47 Hz), a two-stage envelope so the body
//     has both a slam and a tail, tanh saturation ON THE KICK VOICE (never
//     the master) for the harmonics that make a synth kick read as "electro",
//     a real transient made of two decaying blips, and a separate 44 Hz sub
//     layer for weight. POW without a single sample of master clipping.
//
// (4) BEEPS, BOPS, CLICKS, TAPS. A fifth bus of small electronic sounds:
//     real DTMF pairs (which is what makes a beep read as a PHONE), UI bops
//     that drop a fifth in 60 ms, 4 ms clicks and short woody taps. Placed
//     narratively, never as a wall — the signal layer dials in act I, ticks
//     under acts II–III, GOES SILENT in act IV, beeps SOS back in act V,
//     becomes a switchboard in act VI, and is the last thing you hear.
//
// Every rule from the previous four versions survives: 10 ms raised-cosine
// tails on every voice, ducks that ramp over 9 ms rather than step, sine-bump
// bass, no master tanh, mono-safe equal-power pans with a band-limited
// antisymmetric side return, and a vox bus that rides proud of the bed.
//
//   node pop/cult/bin/render9.mjs            # → out/cult-remix-v10-full.wav
//   node pop/cult/bin/render9.mjs --stems    # + per-bus stems
//   ./pop/cult/bin/cut-v5.sh                 # master + mp3s

import { existsSync, mkdirSync, readdirSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const BPM = 120;
const BEAT = 60 / BPM;               // 0.5 s
const BAR = 4 * BEAT;                // 2.0 s
const BARS = 112;                    // 224 s = 3:44 full · 3:28 shipped
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

// Five buses now. Drums never duck. The bed takes the breath. The sung
// voices take a quarter-depth duck. The TUBE bus — the held dashes — takes
// the deep kick+snare pump, because that is what @jeffrey asked to pump.
// The SIGNAL bus (beeps/clicks/taps) takes a half-depth duck so the phone
// ticks sit inside the groove rather than on top of it.
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const tubeL = new Float32Array(N), tubeR = new Float32Array(N);
const sigL = new Float32Array(N), sigR = new Float32Array(N);
const sideB = new Float32Array(N);   // bed's side field
const sideV = new Float32Array(N);   // voices'
const sideT = new Float32Array(N);   // the tube's — pumps with it
const sideS = new Float32Array(N);   // the signal layer's
const dlySend = new Float32Array(N);

const VOXG = 1.42;                   // +3 dB on the sung bus (v2's measurement)
const TUBEG = 1.00;                  // the pumped tube bus, levelled by stem measurement
const SIGG = 2.60;                   // the signal layer, levelled by stem measurement
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const hz = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;
const smooth = (u) => (u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u));
// Repeated instruments breathe in phrases instead of landing at one machine
// velocity. This curve is deterministic (so the render remains reproducible),
// but its two mismatched periods keep guitar and accordion from tracing the
// same level arc. Callers still own the musical baseline and accent shape.
const phraseLevel = (bar, phase = 0, depth = 0.18) => clamp(
  1 + depth * (0.64 * Math.sin(TAU * bar / 9 + phase)
    + 0.36 * Math.sin(TAU * bar / 5 + phase * 1.7)),
  0.72, 1.14,
);

// ── the receipt's event array ─────────────────────────────────────────
// Every scored hit is pushed here from inside the voice that makes it, so
// the video renderer can draw the exact same performance the ear hears —
// including WHO sang each note, because three people on one pitch 28 ms
// apart is this track's signature and it should be visible, not only
// audible. Written to out/cult-remix-v10.events.json as `events[]`.
// Debug: subtract a voice to identify it by ear. MUTE=stab node bin/render9.mjs
const MUTED = new Set((process.env.MUTE || "").split(",").map((v) => v.trim()).filter(Boolean));
// ONLY=skid renders that voice alone — the fastest way to hear what one
// layer actually sounds like without a four-minute argument about spectra.
const ONLY = new Set((process.env.ONLY || "").split(",").map((v) => v.trim()).filter(Boolean));
const allow = (v) => !MUTED.has(v) && (!ONLY.size || ONLY.has(v));
if (MUTED.size) console.log("  MUTED:", [...MUTED].join(", "));
if (ONLY.size) console.log("  ONLY:", [...ONLY].join(", "));
const EVENTS = [];
const PCN = { c: 0, cs: 1, d: 2, ds: 3, e: 4, f: 5, fs: 6, g: 7, gs: 8, a: 9, as: 10, b: 11 };
const pitchMidi = (tok) => {
  const m = /^([a-g]s?)(-?\d)$/.exec(tok || "");
  return m ? PCN[m[1]] + 12 * (+m[2] + 1) : null;
};
function classify(name) {
  let m;
  if ((m = /^dash-(camille|alex|jeffrey)-([a-g]s?\d)-hold$/.exec(name)))
    return { voice: "dash", who: m[1], midi: pitchMidi(m[2]) };
  if ((m = /^dashlong-(camille|jeffrey)-([a-g]s?\d)$/.exec(name)))
    return { voice: "dash", who: m[1], midi: pitchMidi(m[2]) };
  if ((m = /^cult(?:long)?-([a-g]s?\d)$/.exec(name)))
    return { voice: "cult", who: "camille", midi: pitchMidi(m[1]) };
  if ((m = /^voxdot-(c|a|j)-([a-g]s?\d)$/.exec(name)))
    return { voice: "dot", who: { c: "camille", a: "alex", j: "jeffrey" }[m[1]], midi: pitchMidi(m[2]) };
  if (/^altdot-\d+-long$/.test(name)) return { voice: "dot", who: null, midi: null };
  if ((m = /^sos-dash-([a-g]s?\d)$/.exec(name)))
    return { voice: "sosdash", who: "camille", midi: pitchMidi(m[1]) };
  if ((m = /^bassdash-([a-g]s?\d)$/.exec(name)))
    return { voice: "bassdash", who: "jeffrey", midi: pitchMidi(m[1]) };
  if ((m = /^dot-(c|a|j)-([a-g]s?\d)$/.exec(name)))
    return { voice: "dot", who: { c: "camille", a: "alex", j: "jeffrey" }[m[1]], midi: pitchMidi(m[2]) };
  if ((m = /^dot-([a-g]s?\d)$/.exec(name)))
    return { voice: "dot", who: null, midi: pitchMidi(m[1]) };
  if (/^(runrealfast|runitfast|iwanna|away|hideaway|dotdotdash|threeofus)/.test(name))
    return { voice: "lead", who: null, midi: null };
  if (/^hat[CO]$/.test(name)) return { voice: "hat" };
  if (/^(clap|snap|snare|block|tambo|ride)$/.test(name)) return { voice: "perc" };
  if (name === "sweep") return { voice: "sweep" };
  if (/^(chant-full|tagline|hook-spoken|three-of-us)$/.test(name)) return { voice: "source" };
  if (name.startsWith("alt-")) return { voice: "alt" };
  if (name.startsWith("phone-")) return { voice: "phone" };
  return { voice: "sample" };
}

// The click amnesty — every voice exits through 10 ms of raised cosine.
const tailFade = (i, n) => {
  const u = (n - 1 - i) / (0.010 * SR);
  return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
};

// ── sample bank ───────────────────────────────────────────────────────
const BANK = {};
function load(name, path) {
  if (!existsSync(path)) { console.warn(`  ! missing sample ${path}`); return; }
  let p = path;
  if (!path.endsWith(".wav")) {
    p = join(OUT, `.cache-${name}.wav`);
    if (!existsSync(p))
      execFileSync("ffmpeg", ["-y", "-v", "error", "-i", path, "-ac", "1", "-ar", String(SR), p]);
  }
  const { samples, sampleRate } = readWavMono(p);
  if (sampleRate !== SR) {
    const q = join(OUT, `.cache-${name}-rs.wav`);
    execFileSync("ffmpeg", ["-y", "-v", "error", "-i", p, "-ac", "1", "-ar", String(SR), q]);
    BANK[name] = readWavMono(q).samples;
  } else BANK[name] = samples;
  const s = BANK[name];
  let a = 0; while (a < s.length - 1 && Math.abs(s[a]) < 0.008) a++;
  const t = s.subarray(Math.max(0, a - Math.round(0.002 * SR)));
  let peak = 0; for (const v of t) peak = Math.max(peak, Math.abs(v));
  const g = peak > 1e-6 ? 0.95 / peak : 1;
  const out = new Float32Array(t.length);
  for (let i = 0; i < t.length; i++) out[i] = t[i] * g;
  BANK[name] = out;
}

// samples/ = v1's slices; sung/ = the speech-to-singing renders; alt/samples
// = anything harvested from the OTHER cult-tagged whistlegraph posts (see
// alt/harvest.json); phone/ = real telephone gestures, CC0 from freesound
// (bin/phones.sh rebuilds them from the manifest). alt/ and phone/ are
// optional — the score checks before it plays.
for (const dir of ["samples", "sung", "alt/samples", "phone"]) {
  const d = resolve(LANE, dir);
  if (!existsSync(d)) continue;
  for (const f of readdirSync(d)) if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(d, f));
}
const has = (n) => !!BANK[n];

const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({
  hatC: "perc-hat-c.mp3", hatO: "perc-hat-o.mp3", clap: "perc-clap.mp3",
  ride: "perc-ride.mp3", snap: "perc-snap.mp3", snare: "perc-snare.mp3",
  tambo: "perc-tambo.mp3", block: "perc-block.mp3", sweep: "bed-noise-sweep.mp3",
})) load(n, join(DEMOS, f));

// ── space ─────────────────────────────────────────────────────────────
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}

function emit(bus, i, mono, pan, sp, sideAmt, dly = 0) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const cl = Math.cos(a), cr = Math.sin(a);
  if (bus === "drums") { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
  else if (bus === "vox") { voxL[i] += mono * cl; voxR[i] += mono * cr; }
  else if (bus === "tube") { tubeL[i] += mono * cl; tubeR[i] += mono * cr; }
  else if (bus === "sig") { sigL[i] += mono * cl; sigR[i] += mono * cr; }
  else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
  if (dly) dlySend[i] += mono * dly;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    const s = 0.5 * (l - r) * sideAmt;
    if (bus === "vox") sideV[i] += s;
    else if (bus === "tube") sideT[i] += s;
    else if (bus === "sig") sideS[i] += s;
    else sideB[i] += s;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
// Two envelopes, both built AFTER scoring from the triggers the score
// pushed, both ramping to the floor over ~9 ms and recovering on a
// smoothstep. A stepped duck is a click with extra steps.
const kicks = [], snares = [];
function buildEnv(triggers) {
  const e = new Float32Array(N).fill(1);
  const pre = Math.round(0.010 * SR);
  for (const { t, depth, atk, rel } of triggers) {
    const i0 = Math.round(t * SR) - pre;
    const span = pre + Math.round((atk + rel) * SR);
    for (let i = 0; i < span; i++) {
      const j = i0 + i;
      if (j < 0) continue;
      if (j >= N) break;
      const dt = i / SR - 0.010;
      let g;
      if (dt < atk) g = 1 - depth * clamp((dt + 0.010) / (0.010 + atk), 0, 1);
      else { const u = clamp((dt - atk) / rel, 0, 1); g = (1 - depth) + depth * smooth(u); }
      if (g < e[j]) e[j] = g;
    }
  }
  return e;
}

// ── (3) THE THICK ELECTRO KICK ────────────────────────────────────────
// @jeffrey: "want a thicker kick / POW / like a nice electro synth keek".
//
// Where it comes from, in order of how much it matters:
//   · the SWEEP is deeper and faster — 200 Hz down to 47 Hz with a 62/s
//     decay, so the pitch drop happens inside the first 20 ms and reads as
//     one event rather than a bloop;
//   · the ENVELOPE is two-stage: a 34/s slam stacked on a 7/s tail, which
//     is what makes a kick sound big and short at the same time;
//   · SATURATION on the kick's own waveform (tanh at drive 2.4, normalised)
//     folds in the odd harmonics that let a sine kick cut on small speakers.
//     This is the ONLY tanh in the render and it is nowhere near the master;
//   · a defined TRANSIENT: two decaying blips at 1.6 k and 3.9 kHz, ~4 ms,
//     so there is an attack to hear;
//   · a separate 44 Hz SUB layer with a slow decay for the weight under it.
//
// The kick is still not allowed to make the master clip. Everything above
// is envelope, harmonics and voice-level saturation — the drum bus level is
// unchanged, and the master still sums clean and gets ONE static trim.
const SATN = Math.tanh(2.4);
function kick(t, gain = 1, { weight = 1 } = {}) {
  kicks.push(t);
  EVENTS.push({ t: +t.toFixed(4), voice: "kick", bus: "drums", dur: 0.52, gain: +gain.toFixed(3), weight });
  const n = Math.round(0.52 * SR), i0 = Math.round(t * SR);
  let ph = 0, sub = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 47 + 153 * Math.exp(-u * 62);
    ph += (TAU * f) / SR;
    sub += (TAU * 44) / SR;
    const slam = Math.exp(-u * 34), tail = Math.exp(-u * 7.0);
    const env = (0.62 * slam + 0.52 * tail) * Math.min(1, u / 0.0009);
    const body = Math.tanh(Math.sin(ph) * env * 2.4) / SATN;
    const low = Math.sin(sub) * Math.exp(-u * 5.6) * 0.40 * weight;
    const click = Math.exp(-u * 300) * 0.13 * Math.sin(TAU * 1600 * u)
      + Math.exp(-u * 760) * 0.075 * Math.sin(TAU * 3900 * u);
    emit("drums", i0 + i, (body + low + click) * 0.74 * gain * tailFade(i, n), 0, null, 0);
  }
}

// A quiet snare/rim on 3 — mostly there so the tubular pump has a second
// trigger, which is what makes the pump read as rhythm rather than as the
// kick's shadow.
// v10.1: @jeffrey — "bring in kick reverse wub wub kicks too, like we
// have on other /pop tracks at times". Two re-entry voices: revKick is
// the thick kick's sweep run BACKWARDS — pitch and amplitude rising into
// the downbeat it announces — and wub is the root's sub octave pumped by
// a 4 Hz amplitude-and-lowpass LFO, the kick's tail learning to talk.
function revKick(t, dur = 0.9, gain = 0.5) {
  if (!allow("kick")) return;
  EVENTS.push({ t: +(t - dur).toFixed(4), voice: "revkick", bus: "drums",
    dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: 0 });
  const n = Math.round(dur * SR), i0 = Math.round((t - dur) * SR);
  let p = 0;
  for (let i = 0; i < n; i++) {
    const u = i / n;
    const f = 47 + (200 - 47) * u * u;
    p += (TAU * f) / SR;
    const env = Math.pow(u, 2.4);
    const v = Math.tanh(Math.sin(p) * (1 + 1.5 * u)) * 0.5;
    emit("drums", i0 + i, v * env * gain * tailFade(i, n), 0, null, 0.2);
  }
}
function wub(t, midi, bars = 1, gain = 0.30, rate = 4) {
  if (!allow("bass")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "wub", bus: "music", midi,
    dur: +(bars * BAR).toFixed(3), gain: +gain.toFixed(3), pan: 0 });
  const n = Math.round(bars * BAR * SR), i0 = Math.round(t * SR);
  const f = hz(midi);
  let p = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    p += (TAU * f) / SR;
    const lfo = 0.5 - 0.5 * Math.cos(TAU * rate * u);
    const raw = Math.tanh(Math.sin(p) * (1.2 + 2.2 * lfo));
    const k = 0.12 + 0.55 * lfo;
    lp += k * (raw - lp);
    const env = Math.min(1, u / 0.02);
    emit("music", i0 + i, lp * env * 0.5 * gain * tailFade(i, n), 0, null, 0.2);
  }
}

// v10.1: @jeffrey — "more bass in the 1:10 chants, like heavier bass —
// maybe we can even bring in some electric guitar long decay there". So
// act IV grows the record's only stringed thing: a Karplus-Strong pluck
// through a soft tanh drive, feedback high enough to ring for seconds
// under the flanged chant. Synthesized like everything else — the guitar
// is a wavetable of noise the string forgets slowly.
function guitar(t, midi, dur, gain = 0.5, pan = 0, fb = 0.9965, evVoice = "guitar") {
  if (!allow(evVoice)) return;
  EVENTS.push({ t: +t.toFixed(4), voice: evVoice, bus: "music", midi,
    dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const period = Math.max(2, Math.round(SR / hz(midi)));
  const n = Math.round(dur * SR), i0 = Math.round(t * SR);
  const line = new Float32Array(period);
  // String excitation owns its seed: adding a chord cannot silently change
  // the later drum-friction noise or any other scored gesture.
  let gseed = ((Math.round(t * 1000) * 2654435761) ^ (midi * 2246822519)) >>> 0;
  const grnd = () => {
    gseed ^= gseed << 13; gseed >>>= 0;
    gseed ^= gseed >>> 17;
    gseed ^= gseed << 5; gseed >>>= 0;
    return (gseed / 4294967295) * 2 - 1;
  };
  for (let k = 0; k < period; k++) line[k] = grnd();
  const sp = spatial(pan * 1.2);
  let idx = 0, prev = 0;
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const v = line[idx];
    line[idx] = fb * 0.5 * (v + prev);
    prev = v;
    idx = (idx + 1) % period;
    // two-stage asymmetric drive (real distortion, even harmonics), then
    // a dark one-pole so the fuzz reads as weight rather than edge
    const d1 = Math.tanh(v * 6.5 + 0.35) - Math.tanh(0.35);
    const d2 = Math.tanh(d1 * 1.8) * 0.5;
    lp += 0.16 * (d2 - lp);
    const env = Math.min(1, i / (0.002 * SR));
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, 0.4, 0.20);
  }
}

// A guitar chord is four separately modelled strings, not a pad with a
// guitar label. The 17 ms rake makes the hand audible; reversing the order
// on alternating hits gives the progression down-strokes and up-strokes.
function guitarChord(t, midis, dur = 2.8, gain = 0.24, pan = 0, up = false) {
  const notes = up ? [...midis].reverse() : midis;
  for (let k = 0; k < notes.length; k++)
    guitar(t + k * 0.017, notes[k], dur - k * 0.025,
      gain * (k === 0 ? 1 : 0.72 + 0.055 * Math.sin(t * 2.3 + k * 1.9)),
      pan + (k - 1.5) * 0.055, 0.9972, "guitar-chord");
}

// Short B-minor runs tear out of the chord wall at the ends of phrases.
// They accelerate into the downbeat, then the final note rings across it.
function guitarShred(t, notes, gain = 0.17, pan = 0.34) {
  let u = t;
  for (let k = 0; k < notes.length; k++) {
    const last = k === notes.length - 1;
    const bow = 0.76 + 0.24 * Math.sin(Math.PI * k / Math.max(1, notes.length - 1));
    const accent = k % 3 === 2 ? 1.08 : 0.94;
    guitar(u, notes[k], last ? 1.45 : 0.30, gain * bow * accent * (last ? 1.22 : 1),
      pan * (k & 1 ? -1 : 1), last ? 0.9974 : 0.9948, "guitar-shred");
    u += (0.115 - 0.0045 * k) * BEAT;
  }
}

function snare(t, gain = 1) {
  snares.push(t);
  // perc-block is a near-pure 2502 Hz sine (tonal prominence x14523). 126 of
  // them across the record read as one high buzz that never stops — the thing
  // @jeffrey kept hearing "come in with the harmonies". A tap is a transient,
  // not a pitch, so this is a short noise burst with a woody body instead.
  woodTap(t, 0.17 * gain, 0.26);
}

// Sine bumps: fundamental + sub octave + a whisper of the 2nd.
function bass(t, midi, dur, gain = 1, slideFrom = null) {
  if (!allow("bass")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bass", bus: "music", midi, dur: +dur.toFixed(3),
    gain: +gain.toFixed(3), ...(slideFrom !== null ? { slideFrom } : {}) });
  const n = Math.round((dur + 0.12) * SR), i0 = Math.round(t * SR);
  const f1 = hz(midi), f0 = slideFrom !== null ? hz(slideFrom) : f1;
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const glide = smooth(clamp(u / 0.075, 0, 1));
    const f = f0 + (f1 - f0) * glide;
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = Math.min(1, u / 0.012);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.12);
    const s = Math.sin(p1) + 0.52 * Math.sin(p2) + 0.10 * Math.sin(p3);
    lp += 0.50 * (s - lp);
    emit("music", i0 + i, lp * 0.40 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

// Harmonized sines — the pad and the stab.
function sines(t, midis, dur, gain, pan, sideAmt = 0.5, bright = 1, dly = 0, attack = 0.020) {
  if (!allow(dur > 1 ? "pad" : "stab")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: dur > 1 ? "pad" : "stab", bus: "music", midis,
    dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const n = Math.round((dur + 0.40) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const ph = midis.map(() => [0, 0, 0]);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    let s = 0;
    for (let v = 0; v < midis.length; v++) {
      const f = hz(midis[v]);
      ph[v][0] += (TAU * f) / SR;
      ph[v][1] += (TAU * f * 1.0035) / SR;
      ph[v][2] += (TAU * f * 2) / SR;
      s += Math.sin(ph[v][0]) + 0.7 * Math.sin(ph[v][1]) + 0.08 * bright * Math.sin(ph[v][2]);
    }
    s /= midis.length * 1.8;
    lp += 0.28 * (s - lp);
    let env = Math.min(1, u / attack);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.40);
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, sideAmt, dly);
  }
}

// ── (4) THE SIGNAL LAYER ──────────────────────────────────────────────
// "more phone beeps and bops / and clicks and taps".
//
// A beep reads as a PHONE when it is two tones, not one — so these are real
// DTMF pairs off the keypad. The record dials C-U-L-T, which on a phone is
// 2-8-5-8, in act I. Everything here exits through a 12 ms raised-cosine
// release and enters over 3 ms, so the busiest sixteenth of taps still
// cannot make a click.
const DTMF_ROW = [697, 770, 852, 941];
const DTMF_COL = [1209, 1336, 1477, 1633];
const KEY = {
  1: [0, 0], 2: [0, 1], 3: [0, 2], 4: [1, 0], 5: [1, 1], 6: [1, 2],
  7: [2, 0], 8: [2, 1], 9: [2, 2], "*": [3, 0], 0: [3, 1], "#": [3, 2],
};
const CULT_DIAL = ["2", "8", "5", "8"];     // C · U · L · T

function beep(t, f1, f2, dur, { gain = 1, pan = 0, side = 0.6, dly = 0, bus = "sig", digit = null } = {}) {
  if (!allow("beep")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "beep", bus, hz: f2 ? [f1, f2] : [f1], dur: +dur.toFixed(3),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2), ...(digit ? { digit } : {}) });
  const rel = 0.012, n = Math.round((dur + rel) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let p1 = 0, p2 = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    p1 += (TAU * f1) / SR;
    if (f2) p2 += (TAU * f2) / SR;
    const atk = 0.5 - 0.5 * Math.cos(Math.PI * clamp(u / 0.003, 0, 1));
    const off = u > dur ? 0.5 + 0.5 * Math.cos(Math.PI * clamp((u - dur) / rel, 0, 1)) : 1;
    const s = (Math.sin(p1) + (f2 ? 0.85 * Math.sin(p2) : 0)) * (f2 ? 0.5 : 0.9);
    emit(bus, i0 + i, s * atk * off * 0.30 * gain * tailFade(i, n), pan, sp, side, dly);
  }
}
const dtmf = (t, digit, dur, o = {}) => {
  const k = KEY[digit]; if (!k) return;
  beep(t, DTMF_ROW[k[0]], DTMF_COL[k[1]], dur, { ...o, digit: String(digit) });
};

// A "bop": a sine that drops a fifth in 60 ms. UI, not music.
function bop(t, f, { gain = 1, pan = 0, side = 0.5, dur = 0.085, dly = 0 } = {}) {
  if (!allow("bop")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bop", bus: "sig", hz: +f.toFixed(1),
    dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const n = Math.round((dur + 0.02) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let p = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const ff = f * (1 - 0.33 * smooth(clamp(u / 0.060, 0, 1)));
    p += (TAU * ff) / SR;
    const env = Math.min(1, u / 0.002) * Math.exp(-u * 26);
    emit("sig", i0 + i, Math.sin(p) * env * 0.34 * gain * tailFade(i, n), pan, sp, side, dly);
  }
}

// Clicks and taps: bandpassed noise, deterministic (the seed is the post
// date), 4 ms and 18 ms respectively. The tap has a woody 900 Hz body; the
// click is nearly pure edge.
let nseed = 987654321;
// xorshift32, byte-for-byte the generator MenuBandPercussion.swift uses. The
// old LCG multiplied past 2^53 in a double, so its low bits — the ones that
// carry an LCG's randomness — were being rounded away before the truncate.
// It measured nearly white anyway, but this removes the variable entirely.
const nrnd = () => {
  nseed ^= nseed << 13; nseed >>>= 0;
  nseed ^= nseed >>> 17;
  nseed ^= nseed << 5; nseed >>>= 0;
  return (nseed / 4294967295) * 2 - 1;
};
function noiseHit(t, { gain = 1, pan = 0, side = 0.5, dur = 0.004, tone = 0, q = 0.35, dly = 0, evVoice = "click" } = {}) {
  EVENTS.push({ t: +t.toFixed(4), voice: evVoice, bus: "sig", dur: +dur.toFixed(4),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const n = Math.round((dur + 0.006) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let hp = 0, prev = 0, lp = 0, p = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const w = nrnd();
    hp = 0.86 * (hp + w - prev); prev = w;
    lp += 0.42 * (hp - lp);
    let v = lp;
    if (tone) { p += (TAU * tone) / SR; v = v * (1 - q) + Math.sin(p) * q * Math.exp(-u * 120); }
    // 0.9 ms of ramp: still reads as a tick, but the raw sample-to-sample
    // step stays inside what a click scan calls music rather than a fault.
    const env = Math.min(1, u / 0.0009) * Math.exp(-u * (tone ? 150 : 620));
    emit("sig", i0 + i, v * env * 1.10 * gain * tailFade(i, n), pan, sp, side, dly);
  }
}
const click = (t, o = {}) => noiseHit(t, { dur: 0.004, tone: 0, evVoice: "click", ...o });
const tap = (t, o = {}) => noiseHit(t, { dur: 0.018, tone: 900, q: 0.40, evVoice: "tap", ...o });

// A wooden tap: filtered noise through a fast decay with two shallow body
// resonances an octave apart. Broadband enough to never ring as a tone.
function woodTap(t, gain = 0.17, pan = 0.26) {
  if (!allow("tap")) return;
  const n = Math.round(0.075 * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  EVENTS.push({ t: +t.toFixed(4), voice: "tap", bus: "drums", gain: +gain.toFixed(3), pan });
  let lp = 0, b1 = 0, b2 = 0;
  const a1 = 1 - Math.exp((-TAU * 900) / SR), a2 = 1 - Math.exp((-TAU * 1900) / SR);
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const env = Math.exp(-u * 105) * Math.min(1, u / 0.0006);
    const white = nrnd();
    lp += (1 - Math.exp((-TAU * 5200) / SR)) * (white - lp);
    b1 += a1 * (lp - b1); b2 += a2 * (lp - b2);
    const v = b1 * 0.62 + b2 * 0.30 + lp * 0.16;
    emit("drums", i0 + i, v * env * gain * 1.5 * tailFade(i, n), pan, sp, 0.55, 0);
  }
}

// ── TRACKDRUM GEOMETRY — friction that traces a real finger path ──────
// @jeffrey: "along the edge is nice too ... using the geometry of the
// trackdrum and making the gestures weirder ... little spirals and so on".
//
// Menu Band's `setDrumSkinScratch` does not take a cutoff — it takes a POINT
// on the trackpad, and derives the timbre from where that point sits on a
// rounded rectangle: skin in the middle, then snare, rim, hat, and finally
// the bright metal edge. Ported verbatim from MenuBandPercussion.swift:
//   cutoff    175 -> 430 -> 680 -> 1250 -> 2050 Hz  across the material zones
//   resonance  48 ->  90 -> 185 ->  360 ->  560 Hz
// plus seam activity at distances .30/.46/.64/.88 (the seams you can feel),
// a path-position ripple, and a speed->pitch lift. So a gesture is now a
// SHAPE, and the sound follows the hand across the surface.
// Signed distance to the rounded rectangle, normalized 0 (centre) -> 1 (edge).
// At the centre the signed distance is exactly -0.5 with these constants, so
// that is the depth the interior is scaled by.
const surfaceDistance = (x, y) => {
  const sx = (x - 0.5) * 2, sy = (y - 0.5) * 2;
  const hw = 0.82, hh = 0.50, corner = 0.055;
  const qx = Math.abs(sx) * hw - (hw - corner);
  const qy = Math.abs(sy) * hh - (hh - corner);
  const outside = Math.hypot(Math.max(qx, 0), Math.max(qy, 0));
  const inside = Math.min(Math.max(qx, qy), 0);
  const signed = outside + inside - corner;         // <0 inside, 0 on the edge
  const depth = Math.max(0, -signed);               // 0.5 at dead centre
  return Math.max(0, Math.min(1, 1 - depth / 0.5));
};
const sstep = (a, b, v) => { const u = Math.max(0, Math.min(1, (v - a) / (b - a))); return u * u * (3 - 2 * u); };
const mixv = (a, b, m) => a + (b - a) * m;

// The gesture library. Each returns {x,y} in [0,1] for u in [0,1].
const PATHS = {
  // Measured against the compiled Swift: the reference drag reaches surface
  // distance ~0.61 — the RIM, cutoff 680 — and that is what makes it bright
  // (centroid 5.6 kHz). Paths that hug the centre sit on skin at 175 Hz and
  // sound dull, which is what mine were doing. So every path now spends its
  // time out at snare/rim. It stops short of 0.62 on purpose: past there the
  // resonance ramp climbs from 185 toward 560, and THAT is the metallic ring
  // @jeffrey kept hearing. Bright is the cutoff; metallic is the resonance.
  edge: (u) => ({ x: 0.12 + 0.76 * u, y: 0.72 - 0.03 * Math.sin(u * Math.PI * 3) }),
  edgeBack: (u) => ({ x: 0.88 - 0.76 * u, y: 0.28 + 0.03 * Math.sin(u * Math.PI * 3) }),
  spiral: (u) => {
    const a = u * Math.PI * 5.2, r = 0.22 + 0.30 * u;
    return { x: 0.5 + Math.cos(a) * r, y: 0.5 + Math.sin(a) * r * 0.78 };
  },
  spiralIn: (u) => {
    const a = (1 - u) * Math.PI * 5.2, r = 0.52 - 0.30 * u;
    return { x: 0.5 + Math.cos(a) * r, y: 0.5 + Math.sin(a) * r * 0.78 };
  },
  corner: (u) => ({ x: 0.5 + 0.40 * u * u, y: 0.5 + 0.34 * u * u }),
  scrub: (u) => ({ x: 0.14 + 0.72 * u, y: 0.5 + 0.40 * Math.sin(u * Math.PI * 6.5) }),
};

function frictionPath(t, dur, {
  path = "spiral", gain = 1, side = 0.30, dly = 0, rough = 0.55,
  synthetic = false, rel = 0.10, shape = "slide", speed = 1,
} = {}) {
  if (!allow("skid")) return;
  const fn = PATHS[path] || PATHS.spiral;
  const n = Math.round((dur * 0.55 + rel * 0.7 + 0.06) * SR), i0 = Math.round(t * SR);
  const span = dur * 0.55;
  EVENTS.push({ t: +t.toFixed(4), voice: "skid", bus: "drums", shape: `path:${path}`,
    dur: +span.toFixed(3), gain: +gain.toFixed(3), rough });
  const atkA = 1 - Math.exp(-1 / (SR * 0.0025));
  const relA = 1 - Math.exp(-1 / (SR * rel * 0.7));
  let outLp = 0;
  let lvl = 0, nf = 0, ns = 0, ph = 0, px = 0.5, py = 0.5;
  for (let i = 0; i < n; i++) {
    const u = i / SR, x = u / span;
    const k = Math.max(0, Math.min(1, x));
    const pt = fn(k);
    // travel per sample -> the "speed" the Swift voice is driven by
    const dx = pt.x - px, dy = pt.y - py; px = pt.x; py = pt.y;
    const travel = Math.hypot(dx * 1.64, dy) * SR;
    const d = surfaceDistance(pt.x, pt.y);
    // the material ramps, verbatim
    const toSnare = sstep(0.23, 0.31, d), toRim = sstep(0.40, 0.48, d);
    const toHat = sstep(0.62, 0.70, d) * 0.35;   // never fully into the ringing zone
    // The metal-edge zone is what ground; a fingertip on this record never
    // gets there. Capped at a quarter of the way in.
    const toClick = sstep(0.88, 0.965, d) * 0.25;
    // Verbatim from the Swift, and the honest numbers are DULL: skin is
    // 175 Hz. The hat (1250) and metal (2050) zones are dropped entirely —
    // a fingertip on this record never leaves skin/snare/rim, and reaching
    // for them is what made every version sound harsh.
    let cut = mixv(175, 430, toSnare);
    cut = mixv(cut, 680, toRim); cut = mixv(cut, 1250, toHat); cut = mixv(cut, 2050, toClick);
    let res = mixv(mixv(mixv(mixv(48, 90, toSnare), 185, toRim), 360, toHat), 560, toClick);
    // the seams you can feel under a fingertip
    const seam = [0.30, 0.46, 0.64, 0.88].reduce((m, sd) =>
      Math.max(m, Math.exp(-Math.pow((d - sd) / 0.045, 2))), 0);
    const ripple = 1 + 0.055 * Math.sin(((pt.x - 0.5) * 2 * 2.7 + (pt.y - 0.5) * 2 * 3.9) * Math.PI);
    const lift = 1 + Math.min(0.55, travel * 0.00016) * speed;
    // "too harsh": the material ramp reaches 1250 (hat) and 2050 (metal) and
    // the speed lift pushed past both. A fingertip on a head is DULL — so the
    // travelling brightness is kept, but the ceiling comes down to the rim.
    cut = cut * (1 + seam * 0.20) * lift;
    // The resonance is what rings. The audition @jeffrey picked (E) ran at a
    // FIXED 190 Hz; the material ramp climbs to 560 and then the speed-lift
    // and seam terms multiply it again — roughly triple, which with E's
    // carrier-heavy weighting is a metallic tone, not a scuff. Position still
    // colours the sound through `cut`; the resonance is held near the voice
    // he actually chose.
    res = Math.min(255, res * ripple * (1 + seam * 0.06));
    if (synthetic) { cut = 1200 + 9000 * d; res = 1100 + cut * 0.42; }
    // envelope: the hand is on the surface for the whole path
    let target = 0;
    if (x < 1) target = shape === "skid" ? Math.exp(-x * 4.2)
      : shape === "drag" ? Math.pow(x < 0.92 ? x / 0.92 : 1, 1.6)
        : Math.sin(Math.PI * Math.min(1, x));
    target *= 0.55 + 0.45 * Math.min(1, travel * 0.0011);   // faster = louder
    if (shape === "drag") target *= 0.62;    // it lands on the downbeat; stay under it
    lvl += (target > lvl ? atkA : relA) * (target - lvl);
    const fa = 1 - Math.exp((-TAU * cut) / SR);
    const sa = 1 - Math.exp((-TAU * Math.max(35, cut * 0.18)) / SR);
    const white = nrnd();
    nf += fa * (white - nf); ns += sa * (white - ns);
    const band = nf - ns;
    const motion = synthetic ? 1 : 1 + Math.tanh(band * 8) * 0.055;
    ph += (res * motion) / SR; if (ph >= 1) ph -= Math.floor(ph);
    const carrier = Math.sin(TAU * ph);
    const gnarl = Math.tanh(band * (5 + rough * 5));
    const texture = synthetic ? nf * carrier * 1.35
      : gnarl * 0.44 + carrier * (0.08 + Math.abs(gnarl) * (0.42 + rough * 0.30));
    // No output lowpass. Measured against the compiled Swift, the real voice
    // is BRIGHT — centroid 5.6 kHz, 85% rolloff 13.8 kHz — because the
    // friction band is a difference of two one-poles and keeps a long high
    // tail, which tanh then enriches. What makes it a surface instead of a
    // hiss is that it is tiny: the reference peaks at 0.039. Bright and
    // quiet, not dull and loud. That was the whole error.
    const pan = (pt.x - 0.5) * 1.5;            // the hand's position IS the pan
    emit("drums", i0 + i, texture * lvl * 0.15 * gain * tailFade(i, n),
      pan, spatial(pan * 1.2), side, dly);
  }
}

// ── SKIDS AND SLIDES — Menu Band's TrackDrum friction voice ───────────
// @jeffrey: "menu band's trackdrum sliding sound could be GREAT to add —
// some skidding and sliding in the perc".
//
// This is a direct port of the "Continuous membrane friction" block in
// slab/menuband/Sources/MenuBand/MenuBandPercussion.swift (~1296–1338),
// not an approximation of it. The parts that matter, and why:
//
//   · TWO low-pass states over the SAME white sample — one at `cut`, one at
//     max(35, cut*0.18) — and the friction signal is their DIFFERENCE. That
//     is a tight moving band, not broadband noise, and the band IS the skid.
//   · a sine carrier at `res`, frequency-modulated BY THE BAND ITSELF:
//     pitchMotion = 1 + tanh(band*8)*0.055. That 5.5 % self-modulation is
//     the whole reason it sounds like a finger dragging rather than like
//     filtered noise, so it is kept exactly.
//   · nonlinear grip: gnarl = tanh(band*(5 + rough*5)), then
//     texture = gnarl*0.44 + carrier*(0.08 + |gnarl|*(0.42 + rough*0.30)).
//   · a level TARGET through separate attack/release one-poles (0.0025 s
//     physical, 0.006 s synthetic) — so it swells and releases like a real
//     drag instead of gating on. The gesture lives in that envelope.
//
// What v5 adds on top: `cut` and `res` sweep across the gesture, because a
// hand that is sliding is also moving, and a static filter reads as a
// texture rather than a motion. Both stay warm — cutoff 700–2600 Hz,
// carrier 90–420 Hz — so nothing here brightens the top end, and `side` is
// kept low (≤0.34) so the friction never smears the mono fold-down.
//
// Three gestures: `drag` swells INTO a downbeat, `skid` comes off the back
// of a hit, `slide` is a slow swell-and-fall under a transition.
function friction(t, dur, {
  shape = "drag", gain = 1, pan = 0, side = 0.30, dly = 0,
  cut0 = 1300, cut1 = null, res0 = 190, res1 = null,
  rough = 0.55, synthetic = false, rel = 0.10,
} = {}) {
  if (!allow("skid")) return;
  // @jeffrey: "such weird buzzing there ... they should be sounding like light
  // scratches / scuffs ... and be faster". The buzz was the carrier: at
  // rough .55 the old mix put up to .665 of a PURE 90-420 Hz sine into the
  // signal, which reads as a tone, not a surface. A scuff is nearly all
  // friction band and almost no carrier, brighter, and over quickly. So:
  // cutoffs up ~2.3x, carrier up ~3.4x (out of the buzz register and into
  // "scrape"), gestures at 55% length.
  // @jeffrey picked E: the Swift physical weights VERBATIM at drumhead
  // frequencies. My earlier "de-buzz" pass was a misdiagnosis — the buzz was
  // a 2502 Hz near-sine in perc-block (the tap), not this carrier. So the
  // texture and the frequencies are the original TrackDrum voice again, and
  // the only thing kept from that pass is "be faster".
  const SCUFF_CUT = 1.0, SCUFF_RES = 1.0, SCUFF_FAST = 0.55;
  cut0 *= SCUFF_CUT; if (cut1 !== null) cut1 *= SCUFF_CUT;
  res0 *= SCUFF_RES; if (res1 !== null) res1 *= SCUFF_RES;
  dur *= SCUFF_FAST; rel *= 0.7;
  const c1 = cut1 ?? cut0, r1 = res1 ?? res0;
  EVENTS.push({ t: +t.toFixed(4), voice: "skid", bus: "drums", shape, dur: +dur.toFixed(3),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2), cut: [cut0, c1], res: [res0, r1],
    rough, ...(synthetic ? { synthetic: true } : {}) });
  const n = Math.round((dur + rel + 0.06) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const atkA = 1 - Math.exp(-1 / (SR * (synthetic ? 0.006 : 0.0025)));
  const relA = 1 - Math.exp(-1 / (SR * rel));
  let outLp = 0;
  let lvl = 0, nf = 0, ns = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR, x = u / dur;
    let target = 0;
    if (x < 1) {
      target = shape === "skid" ? Math.exp(-x * 4.2)
        : shape === "slide" ? Math.sin(Math.PI * x)
          : Math.pow(smooth(clamp(x / 0.92, 0, 1)), 1.6);      // drag
    }
    lvl += (target > lvl ? atkA : relA) * (target - lvl);
    const k = clamp(x, 0, 1);
    const cut = cut0 + (c1 - cut0) * k;
    const res = res0 + (r1 - res0) * k;
    const fa = 1 - Math.exp((-TAU * cut) / SR);
    const sa = 1 - Math.exp((-TAU * Math.max(35, cut * 0.18)) / SR);
    const white = nrnd();
    nf += fa * (white - nf);
    ns += sa * (white - ns);
    const band = nf - ns;
    const motion = synthetic ? 1 : 1 + Math.tanh(band * 8) * 0.055;
    ph += (res * motion) / SR;
    if (ph >= 1) ph -= Math.floor(ph);
    const carrier = Math.sin(TAU * ph);
    let texture;
    if (synthetic) texture = nf * carrier * 1.35;   // F — the ring-mod mode
    else {
      const gnarl = Math.tanh(band * (5 + rough * 5));
      texture = gnarl * 0.44 + carrier * (0.08 + Math.abs(gnarl) * (0.42 + rough * 0.30));
    }
    emit("drums", i0 + i, texture * lvl * 0.15 * gain * tailFade(i, n), pan, sp, side, dly);
  }
}

// SOS, beeped. This is act V's answer: the same figure the voices sing,
// coming back as a machine.
function beepSOS(t, { gain = 1, pan = 0.5, dly = 0.4, f = 0 } = {}) {
  const row = DTMF_ROW[1], col = f || DTMF_COL[1];
  const seq = [[0.00, 0.09], [0.20, 0.09], [0.40, 0.09],
    [0.70, 0.26], [1.10, 0.26], [1.50, 0.26],
    [1.95, 0.09], [2.15, 0.09], [2.35, 0.09]];
  seq.forEach(([o, d], i) =>
    beep(t + o, row, col, d, { gain: gain * (i < 3 ? 0.9 : i < 6 ? 1.0 : 0.72), pan: pan * (i % 2 ? -1 : 1), side: 0.85, dly }));
}

// ── the one-shot player, with (1) PITCH WIGGLE ────────────────────────
// `wig` is the vibrato depth in cents, `wigHz` its rate, `wigPhase` the
// phase offset that makes three stacked performers beat against each other
// rather than move together, `wigDrift` a slow detune in cents applied over
// the note, and `wigIn` how long the depth takes to arrive. wigIn is the
// whole trick: with the wiggle ramped in, the CONSONANT lands in tune and
// only the held vowel shimmers, so intelligibility survives the effect.
// v10.1: @jeffrey — "the vocal amplitude should slowly rise … it gets a
// bit too loud sometimes … like it needs to stay deep, thick in the
// sounds". A macro arc on the whole vox bus: every sung thing starts at
// ~0.72 of its written gain with a darkness floor, and earns its full
// level and light only as the record approaches the whole message at 76.
// Applied inside shot() so no vocal can escape it; the events receipt
// records the post-arc gain the ear actually gets.
function voxArc(t) {
  const u = smooth(clamp((t / BAR - 8) / (S.whole[0] - 8), 0, 1));
  return { g: 0.72 + 0.28 * u, dark: 0.26 * (1 - u) };
}
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "drums", side = 0.35, dark = 0,
  dur = null, dly = 0, off = 0, evVoice = null, atk = 0.0015,
  wig = 0, wigHz = 5.0, wigPhase = 0, wigDrift = 0, wigIn = 0.45,
} = {}) {
  // @jeffrey: no vocals until 0:42 — the voice bus is silent until the
  // sentence's first dash at the bar-29 hook. (The watery-hole dink rides
  // the drum bus, so it survives.)
  if (bus === "vox" && t < 29 * BAR - 0.02) return;
  // The door (both critics): 400 ms of negative space before the sentence —
  // decorations step aside, the kick keeps the floor.
  if (t >= 29 * BAR - 0.42 && t < 29 * BAR - 0.02) return;
  if (bus === "vox") {
    const arc = voxArc(t);
    gain *= arc.g;
    dark = Math.max(dark, arc.dark);
  }
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  if (!allow(evVoice ?? classify(name).voice)) return;
  const step = Math.pow(2, semis / 12);
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  let avail = Math.floor((s.length - 2 - start) / step);
  // A wiggling read can run ahead of a flat one, so leave it room: the worst
  // case here is well under 3 %, and a sample that runs out mid-read would
  // exit without its tail fade, which is the one thing we never allow.
  if (wig || wigDrift) avail = Math.floor(avail * 0.965);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  if (n <= 4) return;
  const i0 = Math.round(t * SR);
  const c = classify(name);
  EVENTS.push({
    t: +t.toFixed(4), voice: evVoice ?? c.voice, bus, sample: name,
    dur: +(n / SR).toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2),
    ...(c.who ? { who: c.who } : {}),
    ...(c.midi != null ? { midi: c.midi + semis } : semis ? { semis } : {}),
    ...(wig ? { wiggleCents: +wig.toFixed(1), wiggleHz: wigHz, wiggleDriftCents: +wigDrift.toFixed(1) } : {}),
  });
  const sp = spatial(pan * 1.2);
  const span = n / SR;
  const ramp = Math.max(1e-4, wigIn);
  let lp = 0, pos = start;
  for (let i = 0; i < n; i++) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = smooth(Math.min(1, i / (atk * SR)));
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly);
    if (wig || wigDrift) {
      const u = i / SR;
      const d = smooth(u / ramp);
      const cents = d * wig * Math.sin(TAU * wigHz * u + wigPhase)
        + wigDrift * smooth(u / span);
      pos += step * Math.pow(2, cents / 1200);
    } else pos += step;
  }
}
const missing = new Set();
const sung = (name, t, o = {}) => shot(name, t, { bus: "vox", side: 0.6, ...o });

// ── subharmonic doubling ──────────────────────────────────────────────
// @jeffrey: "pitch some voices down like jeffrey's voice — we could add
// subharmonic doubling too". The same take read an octave (or a fifth)
// below itself, darkened and set back, so the voice grows a floor under it
// rather than a second singer beside it. Jeffrey is already the lowest of
// the three, so his takes carry it deepest; the upper voices get a lighter
// touch. `octaves: -12` is the sub, `-19` reaches for the fifth below that.
function subDouble(name, t, o = {}, { amount = 0.34, semis = -12, dark = 0.42, delay = 0.012 } = {}) {
  if (amount <= 0) return;
  sung(name, t + delay, {
    ...o,
    gain: (o.gain ?? 1) * amount,
    semis: (o.semis ?? 0) + semis,
    dark: Math.max(o.dark ?? 0, dark),
    side: Math.min(o.side ?? 0.4, 0.30),      // keep the floor near the middle
    pan: (o.pan ?? 0) * 0.35,
    evVoice: "sub",
  });
}
// Sing a take and give it its own floor in one call.
function sungSub(name, t, o = {}, sub = {}) {
  sung(name, t, o);
  subDouble(name, t, o, sub);
}
// A held dash goes to the tube bus — resonated, saturated, deeply pumped.
const held = (name, t, o = {}) => shot(name, t, { bus: "tube", side: 0.7, ...o });

// ── voice as material ─────────────────────────────────────────────────
function material(bar, name, g = 1, {
  steps = 16, grain = 0.070, dark = 0.18, side = 0.85, dly = 0.22, span = 0.9,
} = {}) {
  const src = BANK[name];
  if (!src) { missing.add(name); return; }
  const len = src.length / SR;
  const tri = triad(degAt(bar));
  for (let k = 0; k < steps; k++) {
    if ((k * 7 + bar * 3) % 5 === 0) continue;
    const t = at(bar) + k * (BEAT / 4);
    const off = ((k * 5 + bar * 3) % 11) / 11 * Math.max(0, len - grain) * span;
    const semis = tri[(k + bar) % tri.length] - tri[0] + (k % 8 === 4 ? 12 : 0);
    const accent = k % 4 === 0 ? 1.0 : k % 2 === 0 ? 0.72 : 0.52;
    shot(name, t, {
      bus: "vox", gain: 0.30 * g * accent, semis, evVoice: "material",
      pan: (k % 2 ? 0.55 : -0.55) * (0.6 + 0.4 * ((k % 3) / 2)),
      side, dark, dly, off, dur: grain * (k % 4 === 0 ? 1.6 : 1),
    });
  }
}

function stretched(name, t, { gain = 1, pan = 0, semis = 0, stretch = 1, dur = 1, side = 0.7, dark = 0.45, bus = "vox", dly = 0 } = {}) {
  if (bus === "vox" && t < 29 * BAR - 0.02) return;  // same hold as shot()
  EVENTS.push({ t: +t.toFixed(4), voice: "stretch", bus, sample: name, dur: +dur.toFixed(3),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2), stretch });
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const step = Math.pow(2, semis / 12);
  const n = Math.round(dur * SR);
  const grain = Math.round(0.055 * SR), hopOut = grain >> 1, hopIn = (hopOut * step) / stretch;
  const acc = new Float64Array(n + grain);
  let read = 0;
  for (let g = 0; g * hopOut < n; g++) {
    if (read + grain * step >= s.length - 2) break;
    for (let k = 0; k < grain; k++) {
      const o = g * hopOut + k;
      if (o >= acc.length - 1) break;
      const pos = read + k * step, q = pos | 0, f = pos - q;
      acc[o] += (s[q] + (s[q + 1] - s[q]) * f) * (0.5 - 0.5 * Math.cos((TAU * k) / (grain - 1)));
    }
    read += hopIn;
  }
  const i0 = Math.round(t * SR), sp = spatial(pan * 1.3);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    lp += (1 - dark) * (acc[i] - lp);
    let env = Math.min(1, u / 0.030);
    const left = (n - i) / SR;
    if (left < 0.12) env *= left / 0.12;
    emit(bus, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, side, dly);
  }
}

// ── harmony ───────────────────────────────────────────────────────────
// B natural minor, four rows of PROGRESSIONS_CHILL (recap/bin/trance.mjs),
// two bars a chord, 32 bars before it comes round.
//
// v5 narrative amendment: act VII — "the whole message" — pins the harmony
// to the home progression Bm · D · G · Em. The chords have been travelling
// for two and a half minutes; when the complete lyric finally arrives, the
// harmony arrives with it. That is the only place in the record where the
// progression stops wandering.
const SCALE = [0, 2, 3, 5, 7, 8, 10];
const sd = (i) => SCALE[((i % 7) + 7) % 7] + 12 * Math.floor(i / 7);
const ROWS = [[0, 5, 2, 6], [0, 6, 3, 5], [0, 4, 5, 3], [0, 2, 5, 3]];
const HOME = [0, 2, 5, 3];                       // Bm · D · G · Em
const bassRoot = (deg) => 35 + sd(deg);          // B1 = 35
const triad = (deg, base = 59) => [sd(deg), sd(deg + 2), sd(deg + 4)].map((s) => base + s);

const CULT_PITCH = { b2: 47, d3: 50, fs3: 54, g3: 55, a3: 57, b3: 59, cs4: 61, d4: 62, e4: 64, fs4: 66, g4: 67 };
function choirFor(deg) {
  const pcs = new Set(triad(deg).map((m) => ((m % 12) + 12) % 12));
  const ok = Object.keys(CULT_PITCH)
    .filter((n) => pcs.has(CULT_PITCH[n] % 12))
    .sort((a, b) => CULT_PITCH[a] - CULT_PITCH[b]);
  if (!ok.length) return ["b3"];
  const low = ok.filter((n) => CULT_PITCH[n] <= 55);
  const high = ok.filter((n) => CULT_PITCH[n] >= 57);
  const pick = [low[0] ?? high[1] ?? high[0], high[0], high[high.length - 1]];
  return [...new Set(pick.filter(Boolean))];
}

// ── form: the ten acts ────────────────────────────────────────────────
const S = {
  carrier:   [0, 8],      // I    a channel opens — DTMF dials C-U-L-T
  three:     [8, 24],     // II   the three voices, one at a time, then SOS
  message:   [24, 40],    // III  the hook: the message is legible
  secret:    [40, 48],    // IV   THE TURN — kick out, beeps out, "cult"
  reply:     [48, 64],    // V    something answers, in beeps
  spread:    [64, 76],    // VI   it travels; the unison comes apart
  whole:     [76, 96],    // VII  the complete four-line chorus, at home
  recognise: [96, 104],   // VIII peel off
  carrieroff:[104, 112],  // IX   the drone leaves; the beeps outlast it
  // (v10.1: THE HUMANS is gone — @jeffrey: "remove the ending section at
  // 3:00 where it's no longer sung … i don't like when vocals skip
  // aesthetivox and feel too raw". The reveal act was the one place the
  // record played the untreated recording; the record no longer does.)
};

// A family of whole-mix physical gestures. Each impulse gives every bus its
// own mass and direction; the event's frequency, damping, strength and glitch
// define a different impact character. The same table is written into the
// receipt so the score video moves by the identical force.
const ELASTIC_EXPLOSIONS = [
  { bar: 29,  kind: "snap",    duration: 1.35, strength: 0.42, hz: 2.45, damping: 1.85, glitch: 0.72 },
  { bar: 40,  kind: "gravity", duration: 6.20, strength: 0.62, hz: 0.46, damping: 0.34, glitch: 0.16 },
  { bar: 48,  kind: "recoil",  duration: 3.60, strength: 0.78, hz: 1.18, damping: 0.72, glitch: 0.42 },
  { bar: 64,  kind: "rupture", duration: 5.80, strength: 1.24, hz: 0.70, damping: 0.42, glitch: 0.84 },
  { bar: 76,  kind: "blast",   duration: 4.80, strength: 1.00, hz: 0.92, damping: 0.54, glitch: 0.64 },
  { bar: 92,  kind: "shatter", duration: 2.20, strength: 0.90, hz: 2.10, damping: 1.18, glitch: 1.00 },
  { bar: 104, kind: "exhale",  duration: 4.60, strength: 0.34, hz: 0.56, damping: 0.55, glitch: 0.10 },
].map((e) => ({ ...e, t: at(e.bar) }));
for (const e of ELASTIC_EXPLOSIONS)
  EVENTS.push({ t: e.t, voice: "spatial-explosion", bus: "master",
    kind: e.kind, dur: e.duration, strength: e.strength, springHz: e.hz,
    damping: e.damping, glitch: e.glitch });
const ACTS = {
  carrier: "I · CARRIER — a channel opens before anything is said",
  three: "II · THREE VOICES — you learn there are three people before you learn what they say",
  message: "III · THE MESSAGE — the hook, legible, over the thick kick",
  secret: "IV · THE SECRET — the turn: everything stops and the word arrives alone",
  reply: "V · THE REPLY — SOS comes back, beeped, from the other side",
  spread: "VI · IT SPREADS — the unison comes apart; the wiggle is at its widest",
  whole: "VII · THE WHOLE MESSAGE — the complete chorus, and the harmony comes home",
  recognise: "VIII · RECOGNITION — elements peel off one at a time",
  carrieroff: "IX · CARRIER OFF — the drone leaves, the beeps outlast it, then a hang-up",
};
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];
const sectionAt = (bar) => Object.keys(S).find((k) => inS(bar, k)) ?? "carrieroff";
const soloBar = (bar) => bar >= 54 && bar < 56;
const releaseGapBar = (bar) => bar >= 68 && bar < 72;
const dotFieldBar = (bar) => bar >= 72 && bar < 76;
const sparseSpreadBar = (bar) => releaseGapBar(bar) || dotFieldBar(bar);

// Sub-shape inside act II: bars 8–16 are the introductions, 16–24 the SOS.
const introBar = (b) => b >= 8 && b < 16;
const sosBar = (b) => b >= 16 && b < 24;

const kickOn = (b) => !(inS(b, "carrier") || inS(b, "secret") || inS(b, "carrieroff")
  || soloBar(b) || sparseSpreadBar(b));
const hatOn = (b) => kickOn(b);
const dense = (b) => inS(b, "reply") || inS(b, "whole");
const hookSection = (b) => inS(b, "message") || inS(b, "reply") || inS(b, "recognise");

function degAt(bar) {
  if (soloBar(bar)) return 6;                    // exposed A-major pivot
  if (dotFieldBar(bar)) return [5, 3, 0, 6][bar - 72];
  if (inS(bar, "whole")) return HOME[Math.floor(((bar - S.whole[0]) % 8) / 2)];
  return ROWS[Math.floor(bar / 8) % ROWS.length][Math.floor((bar % 8) / 2)];
}

// ── OTHER PEOPLE'S CULTS ──────────────────────────────────────────────
// @jeffrey: "get other samples from the other tiktoks / we have many 'cult'
// tiktok takes". The whistlegraph index (system/public/whistlegraph.org/
// posts.json) tags TWELVE posts with the `cult` work, not one. The other
// eleven were pulled from the assets mirror and chopped into alt/samples/
// (see alt/harvest.json for the transcripts, timings and measured f0).
//
// Ten of them contain a fresh utterance of the word. That is a gift to the
// narrative rather than just extra material: act VI is called IT SPREADS,
// and what spreads is literally this — the same word, said by different
// people in different months across 2022, arriving from the edges of the
// stereo field.
//
// Verified present before use; if alt/ has not been harvested the score
// simply skips these lines.
const ALT_CULTS = [
  ["alt-71018-cult", 0.29, 112], ["alt-70555-cult", 0.37, 119],
  ["alt-71244-cult", 0.41, 229], ["alt-70551-cult", 0.43, 136],
  ["alt-71441-cult", 0.52, 207], ["alt-71195-cult", 0.54, 121],
];
// (v10.1: ALT_SUNG / ALT_THREE left with THE HUMANS act — the raw takes
// they pointed at are exactly the "skips aesthetivox, feels too raw"
// vocals the revision removes.)

// ── THE DOTS, FROM EVERYWHERE ─────────────────────────────────────────
// v10.1: @jeffrey — "in the opening we should have lots of the 'dot dot
// dot' … from all the videos … samples and aesthetivoxed … to blend in
// with the phone tones". Every harvested post that says "dot" is here,
// grouped BY POST so one video can speak a whole morse S in its own voice
// — nine posts, twenty-two takes — and the aesthetivox bank answers with
// the same syllable resynthesized to a pitch. That pairing is how a human
// dot blends into a machine's: the raw take carries the room, the sung
// take carries the chord, and both ride the signal layer's echo and its
// dial-tone pan positions.
const ALT_DOTS = [
  ["alt-70551-dot1", "alt-70551-dot2"],
  ["alt-70555-dot1", "alt-70555-dot2", "alt-70555-dot3"],
  ["alt-71018-dot1", "alt-71018-dot2", "alt-71018-dot3"],
  ["alt-71195-dot1", "alt-71195-dot2", "alt-71195-dot3"],
  ["alt-71244-dot1", "alt-71244-dot2", "alt-71244-dot3"],
  ["alt-71437-dot1", "alt-71437-dot2"],
  ["alt-71441-dot1", "alt-71441-dot2", "alt-71441-dot3"],
  ["alt-71448-dot"],
  ["alt-71560-dot1", "alt-71560-dot2"],
];

// The aesthetivoxed dots — bin/sing.mjs's WORLD renders, one syllable at
// a real pitch per performer — keyed the way choirFor keys the choir:
// keep the bank names whose pitch class sits in the current triad.
const DOT_PITCH = {
  "dot-j-b2": 47, "dot-j-d3": 50, "dot-j-e3": 52, "dot-j-fs3": 54,
  "dot-j-g3": 55, "dot-j-a3": 57,
  "dot-a-fs3": 54, "dot-a-g3": 55, "dot-a-a3": 57, "dot-a-b3": 59,
  "dot-a-d4": 62, "dot-a-e4": 64,
  "dot-c-b3": 59, "dot-c-cs4": 61, "dot-c-d4": 62, "dot-c-e4": 64,
  "dot-c-fs4": 66, "dot-c-g4": 67, "dot-c-a4": 69,
};
function dotsFor(deg) {
  const pcs = new Set(triad(deg).map((m) => ((m % 12) + 12) % 12));
  const ok = Object.keys(DOT_PITCH)
    .filter((n) => has(n) && pcs.has(DOT_PITCH[n] % 12))
    .sort((a, b) => DOT_PITCH[a] - DOT_PITCH[b]);
  return ok.length ? ok : ["dot-b3"];
}

// One video says ONE dot, held long. @jeffrey heard the first pass
// (three-dot morse figures, two or three a bar, DTMF shadows) and called
// it: "too chaotic … wayyyy less of them … all extended a lot … more in
// the background" — then heard the granular stretch and called that too:
// "more smoooooth … lessss choppy in their slowness". So the long dots
// are now REAL held notes: bin/longdots.sh runs each video's take through
// the WORLD chain (sing.py duration control — the vowel sustains, the
// consonant stays a consonant) at its own measured pitch snapped into
// B minor, and this function simply plays that render with a slow wiggle.
// The granular stretch survives only as the fallback for an unbuilt bank.
// Per-video pitch of the altdot-*-long renders (longdots.sh's snap), so
// callers can deepen register-aware: `deep` drops any take above B3 by an
// octave — same pitch class, key-safe, twice the length, half the light.
const ALT_DOT_MIDI = [59, 47, 55, 47, 61, 59, 62, 66, 61];
function dotDrift(t, vid, {
  gain = 0.16, pan = 0, dur = 1.6, stretch = 4.5, dly = 0.5, dark = 0.45, semis = 0, deep = false,
} = {}) {
  const takes = ALT_DOTS[((vid % ALT_DOTS.length) + ALT_DOTS.length) % ALT_DOTS.length].filter(has);
  if (!takes.length) return;
  const long = `altdot-${takes[0].split("-")[1]}-long`;
  const vi = ((vid % ALT_DOTS.length) + ALT_DOTS.length) % ALT_DOTS.length;
  const dsemis = semis + (deep && ALT_DOT_MIDI[vi] >= 59 ? -12 : 0);
  if (has(long)) {
    sung(long, t, {
      gain: gain * 1.15 * vel(0.12), pan, semis: dsemis, side: 0.85, dark, dly,
      wig: 6, wigHz: 0.5 + 0.3 * (vid % 3), wigPhase: vid * 1.9, wigDrift: (vid % 2 ? -4 : 4), wigIn: 0.8,
    });
    return;
  }
  let pick = takes[0];
  for (const nm of takes) if (BANK[nm].length > BANK[pick].length) pick = nm;
  stretched(pick, t, { gain: gain * vel(0.12), pan, semis, stretch, dur, side: 0.85, dark, dly });
}

// …and the aesthetivoxed kind: one performer's dot held at a chord tone.
// The 0.2 s staccato sung/dot-* bank chops when slowed, so longdots.sh
// renders voxdot-* — the same takes through the WORLD chain, held 1.7 s —
// and this picks whichever sits in the current triad. @jeffrey: "lets
// start with just camille and alex on dots … no jeffrey voice" — so the
// drift is Camille and Alex only, everywhere. Stretch fallback only for
// an unbuilt bank.
const VOXDOT_PITCH = {
  "voxdot-j-b2": 47, "voxdot-j-fs3": 54, "voxdot-a-a3": 57,
  "voxdot-a-d4": 62, "voxdot-c-b3": 59, "voxdot-c-fs4": 66,
};
function dotDriftVox(t, bar, {
  gain = 0.14, pan = 0, dur = 1.4, stretch = 3.8, dly = 0.45, dark = 0.35, semis = 0,
} = {}) {
  const pcs = new Set(triad(degAt(bar)).map((m) => ((m % 12) + 12) % 12));
  const long = Object.keys(VOXDOT_PITCH)
    .filter((n) => has(n) && pcs.has(VOXDOT_PITCH[n] % 12))
    .filter((n) => !n.startsWith("voxdot-j"))
    .flatMap((n) => (n.startsWith("voxdot-c") ? [n, n] : [n]));   // "more camille dots"
  if (long.length) {
    const nm = long[Math.abs(bar * 7 + 3) % long.length];
    sung(nm, t, {
      gain: gain * 1.15 * vel(0.12), pan, semis, side: 0.8, dark, dly,
      wig: 5, wigHz: 0.45, wigPhase: bar * 1.3, wigDrift: (bar % 2 ? -3 : 3), wigIn: 0.8,
    });
    return;
  }
  const bank = dotsFor(degAt(bar));
  const nm = bank[Math.min(bank.length - 1, bank.length >> 1)];
  stretched(nm, t, { gain: gain * vel(0.12), pan, stretch, dur, side: 0.8, dark, dly });
}

// v10.1's other ask: @jeffrey — "remove 'i wanna hide away' and 'i wanna
// run real fast' until the end". The loop keeps looping, but before act
// VII its two WORDED lines are withheld: every early statement is
// morse-only — the dash lines, the dot lines — with dot figures standing
// exactly where the words will land. Act VII was always titled "the whole
// message the record has been withholding"; now the record actually
// withholds it, and the first time anyone says what they wanna do is the
// moment the harmony comes home at bar 76.
// The withholding is over, but the lyrics hold until the first hook: the
// loop runs morse-only through bar 27, and the first words are the whole
// sentence at bar 28 (0:40) — dash · i wanna · dash · i wanna · run real
// fast · dot dot dot.
const wordsIn = (bar) => bar >= S.message[0] + 4;

let seed = 20220120;                                   // the cult post date
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
// humanized (live-show pass): scatter 1.7x, velocity spread 1.35x —
// same draw count, the parity stream holds.
const jit = (ms = 6) => ((rnd() - 0.5) * 2 * ms * 1.7) / 1000;
const vel = (spread = 0.20) => 1 - rnd() * spread * 1.35;

// ── (1) THE CROSSOVER WIGGLE ──────────────────────────────────────────
// The depth is a function of WHERE IN THE STORY we are, because the story
// is a unison coming apart: 4 cents when the three of them first land on
// the same syllable in act II, 10 through the message, 16 once it has been
// answered, 22 where it spreads, back to 9 for the last statements.
function wigDepth(bar) {
  const s = sectionAt(bar);
  return { three: 4, message: 10, secret: 7, reply: 16, spread: 22,
    whole: 14, recognise: 9, carrieroff: 5, carrier: 0 }[s] ?? 10;
}

// One syllable, three people, one pitch, 28 ms apart — and now each of them
// wiggling at their own rate and phase, so where they cross they beat.
// Rates are mutually irrational-ish (4.3 / 5.9 / 3.4 Hz) so the beating
// never settles into a pattern the ear can predict.
function dashStack(t, which, G, bar) {
  // The dash layer rides the tube bus, so the vox hold never caught it —
  // no dash before the sentence, on any bus. THE fix.
  if (t < 29 * BAR - 0.02) return;
  const w = wigDepth(bar);
  held(`dash-camille-${which}-hold`, t + 0.000 + jit(3), {
    gain: 0.50 * G, pan: -0.42, side: 0.70, dly: 0.10,
    wig: w, wigHz: 4.3, wigPhase: 0.0, wigDrift: +0.42 * w, wigIn: 0.50,
  });
  held(`dash-alex-${which}-hold`, t + 0.028 + jit(3), {
    gain: 0.47 * G, pan: 0.42, side: 0.70, dly: 0.10,
    wig: w * 1.15, wigHz: 5.9, wigPhase: 2.1, wigDrift: -0.5 * w, wigIn: 0.42,
  });
  const jOpts = {
    gain: 0.58 * G, pan: 0.00, side: 0.38, dark: 0.22, dly: 0.08,
    wig: w * 0.7, wigHz: 3.4, wigPhase: 4.3, wigDrift: +0.3 * w, wigIn: 0.60,
  };
  held(`dash-jeffrey-${which}-hold`, t + 0.056 + jit(3), jOpts);
  // Jeffrey is already the floor of the three; the sub octave under his own
  // dash is what gives the unison a bottom instead of a fourth singer.
  held(`dash-jeffrey-${which}-hold`, t + 0.068 + jit(3), {
    ...jOpts, gain: jOpts.gain * 0.40, semis: -12, dark: 0.46,
    side: 0.24, wig: w * 0.35, evVoice: "sub",
  });
}

// ── THE HOOK, SUNG ────────────────────────────────────────────────────
//   0.00  dash ──────── F#4, 1.50 s   three performers stacked (tube bus)
//   1.50  i wanna       D4 → E4
//   2.00  dash ──────── D4,  1.50 s
//   3.50  i wanna       B3 → C#4
//   4.00  run REAL fast G4 → F#4(2.00 s) → D4     — the long-"real" take
//   6.00  dot           B3     ← the held vowel is STILL RINGING here
//   6.50  dot           F#3
//   7.00  ── rest ──
function hook(bar, { full = true } = {}) {
  const t = at(bar);
  const G = full ? 1.0 : 0.70;
  // The v2 hook, verbatim — @jeffrey pointed at 0:48 of the v2 master:
  // "that is the proper lyrical utterance rhythm there. and word choice."
  // dash F#4 · i wanna (1.50) · dash D4 (2.00) · i wanna (3.50) · run IT
  // fast (4.00, one sung phrase) · dot dot · rest. Plain sung takes.
  // dash-dash melody play: each loop voices its dashes in a different
  // octave shape, and a quick camille arpeggio rings off the first.
  const hv = (bar >> 3) % 3;
  dashStack(t + 0.00, hv === 1 ? "fs3" : "fs4", G, bar);
  shot(`dash-camille-${hv === 1 ? "fs4" : "fs3"}-hold`, t + 0.75, { bus: "tube", gain: 0.26 * G, pan: 0.30, side: 0.70, dly: 0.35, dur: 0.28, atk: 0.006 });
  shot("dash-camille-d4-hold", t + 1.06, { bus: "tube", gain: 0.18 * G, pan: -0.34, side: 0.75, dly: 0.45, dur: 0.22, atk: 0.006 });
  sung("iwanna-a-sung", t + 1.50 + jit(4), { gain: 0.82 * G, pan: -0.18, side: 0.50, dly: 0.20 });
  dashStack(t + 2.00, hv === 2 ? "b3" : hv === 1 ? "d3" : "d4", G * 0.95, bar);
  sung("iwanna-b-sung", t + 3.50 + jit(4), { gain: 0.82 * G, pan: 0.18, side: 0.50, dly: 0.20 });
  // The long-"real" take — run · reeeeeaaaal (2.0 s) · fast. At 2.80 s it
  // rings on under the dot answers at +6.00, which is the point.
  // Withheld before act VII: one aesthetivoxed dot stretched across the
  // slot instead — the length of the line with none of its words.
  if (wordsIn(bar)) {
    // same diction fix as the chorus: syllabic take first, melisma under
    // the "real" word, fixed: the syllabic take SAYS run-real-fast (v2's
    // runitfast take garbled it); the melisma rings under.
    sungSub("runrealfast-hi", t + 4.00 + jit(4), { gain: 1.26 * G, pan: 0.00, side: 0.35, dly: 0.10 }, { amount: 0.30 });
    sung("runrealfast-long-hi", t + 4.80 + jit(4), { gain: 0.50 * G, pan: -0.10, side: 0.60, dly: 0.35 });
  }
  else {
    dotDriftVox(t + 4.00 + jit(20), bar, { gain: 0.30 * G, pan: 0.10, dur: 1.9, stretch: 4.2, dly: 0.35, dark: 0.30 });
    bop(t + 5.50 + jit(4), hz(triad(degAt(bar))[0] + 12),
      { gain: 0.20 * G, pan: -0.28, side: 0.7, dly: 0.40 });
  }
  sung("dot-b3", t + 6.00 + jit(3), { gain: 0.90 * G, pan: -0.45, side: 0.75, dly: 0.38, atk: 0.025 });
  sung("dot-fs3", t + 6.50 + jit(3), { gain: 0.90 * G, pan: 0.45, side: 0.75, dly: 0.38, atk: 0.025 });
  // dot dot dot dot — four, ending every loop.
  sung("dot-d4", t + 7.00, { gain: 0.80 * G, pan: 0.0, side: 0.60, dly: 0.45 });
  sung("dot-fs3", t + 7.50, { gain: 0.62 * G, pan: -0.20, side: 0.70, dly: 0.50 });
  // length-play: every other full hook lets a daaaaash ring across the
  // rest bar, stretched to twice itself.
  if (full && ((bar >> 3) & 1))
    stretched("dash-camille-fs4-hold", t + 7.00, { gain: 0.28 * G, pan: -0.30, stretch: 1.9, dur: 3.0, side: 0.80, dark: 0.25, bus: "vox", dly: 0.50 });
}

// ── ACT VII · THE WHOLE MESSAGE ───────────────────────────────────────
// The four-line chorus @jeffrey dictated for v3, which v5 withholds until
// 2:32 on purpose: the record has been transmitting fragments for two and a
// half minutes and this is the message arriving intact.
//
//   0.00  run real fast          G4 → F#4 → D4          (Bm)
//   2.00  i wanna hide           D4 → E4 → F#4          (D)
//   3.00  a — waaaay             G4 → A4 held 1.60      (D)
//   4.00  i wanna                B3 → C#4               (G)
//   4.50  dash ───────────────── D4 held 1.50           (G)
//   6.00  dot · dot              B3 · G3                (Em)
//   7.00  dash ───────────────── B3 held 1.50           (Em)
// v9: the statement takes v3's CHORUS_TAKES grammar — `lead` picks the
// octave of the sung lines ("hi" / "lo" / "both"), `drop` removes lines,
// `fast` swaps line 1 for its double-time self, `answer` puts a figure in
// the statement's holes (v3's "no two alike" rule), `tag` punctuates the
// last bar, and `choirUnder` seats the held "cult" chord beneath the
// fullest statements. Ghost statements are just low `g` with lines dropped.
function chorus(bar, {
  lead = "both", fast = false, g = 1, drop = [],
  answer = "none", tag = null, choirUnder = false,
} = {}) {
  const t = at(bar), G = g;
  const gone = new Set(drop);
  const both = lead === "both", lo = lead === "lo";

  // ── line 1 · "run real fast" ──────────────────────────────────────────
  // (withheld before act VII: one stranger's dot, stretched across the
  // slot where the words will land — background, not statement)
  if (!gone.has(1) && !wordsIn(bar)) {
    dotDrift(t + 0.30 + jit(20), bar >> 3, {
      gain: 0.26 * G, pan: -0.30, dur: 1.5, stretch: 4.0, dly: 0.40, dark: 0.35,
    });
  } else if (!gone.has(1)) {
    if (fast) {
      for (const [k, oct] of [[0.00, "hi"], [1.00, "lo"]])
        sung(`runrealfast-fast-${oct}`, t + k + jit(3),
          { gain: (oct === "hi" ? 1.25 : 0.95) * G, pan: oct === "hi" ? -0.12 : 0.12, side: 0.42, dly: 0.12 });
    } else if (lo) {
      sung("runrealfast-long-lo", t + jit(4), { gain: 1.30 * G, pan: 0.00, side: 0.40, dly: 0.12 });
    } else {
      // v10.1 fourth pass. @jeffrey, three listens running: "still can't
      // hear the run real fast words". The level was never the problem —
      // the LONG take is a melisma ("run reeeeaaal faaast", 2.3 s of held
      // vowel, barely a consonant in it), so every boost was boosting a
      // drone. The syllabic take SAYS the line now, dry and center, and
      // the melisma enters under it a beat later as the vowel it always
      // was.
      sungSub("runrealfast-hi", t + jit(4), { gain: 1.30 * G, pan: 0.00, side: 0.35, dly: 0.10 }, { amount: 0.30 });
      sung("runrealfast-long-hi", t + 0.80 + jit(4), { gain: 0.55 * G, pan: both ? -0.14 : -0.08, side: 0.60, dly: 0.30 });
      if (inS(bar, "reply")) {
        // 1:22 power-up: the line harmonized like the guitar under it —
        // fifth and octave stacked on the melisma. No jit.
        sung("runrealfast-long-hi", t + 0.82, { gain: 0.52 * G, pan: 0.22, side: 0.55, dly: 0.20, semis: 7 });
        sung("runrealfast-long-lo", t + 0.86, { gain: 0.34 * G, pan: -0.24, side: 0.60, dly: 0.28, semis: 12 });
      }
      if (both) sung("runrealfast-long-lo", t + 0.84 + jit(4), { gain: 0.44 * G, pan: 0.16, side: 0.55, dly: 0.28 });
    }
  }

  // ── line 2 · "i wanna hide a — waaaay" ────────────────────────────────
  // (withheld before act VII: one aesthetivoxed dot, stretched where the
  // held vowel would have been, and a bop takes the vowel's peak)
  if (!gone.has(2) && !wordsIn(bar)) {
    dotDriftVox(t + 2.20 + jit(20), bar, { gain: 0.22 * G, pan: 0.24, dur: 1.4, stretch: 3.8, dly: 0.40 });
    bop(t + 3.00 + jit(4), hz(triad(degAt(bar))[1] + 12),
      { gain: 0.22 * G, pan: -0.20, side: 0.7, dly: 0.40 });
  } else if (!gone.has(2)) {
    // v10.1 third pass. The granular stretch smeared the words ("feels
    // broken … kept the 'i wanna', lost the hide away") — so no stretcher
    // anywhere near this line. The hideaway take (no pickup: F#4→G4→A4,
    // its own 1.3 s held tail) sings CLEAN with a resampled sub floor,
    // and the "much longerrrrr" comes from real vowels relaying under it:
    // away-hi picks the vowel up mid-ring, away-lo hands it down the
    // octave, so the waaaay carries four-plus seconds into the dash line
    // without a single grain.
    sungSub("hideaway-hi", t + 2.00 + jit(4), { gain: 0.94 * G, pan: -0.10, side: 0.5, dly: 0.24 }, { amount: 0.30 });
    held("away-hi", t + 3.45 + jit(3), {
      gain: 0.80 * G, pan: -0.06, side: 0.62, dly: 0.26,
      wig: wigDepth(bar) * 0.8, wigHz: 4.9, wigPhase: 1.1, wigDrift: 0.3 * wigDepth(bar), wigIn: 0.55,
    });
    if (both) held("away-lo", t + 4.20 + jit(3), {
      gain: 0.52 * G, pan: 0.22, side: 0.62, dly: 0.26,
      wig: wigDepth(bar) * 0.9, wigHz: 6.3, wigPhase: 3.4, wigDrift: -0.35 * wigDepth(bar), wigIn: 0.48,
    });
  }

  // ── line 3 · "i wanna dash" ───────────────────────────────────────────
  if (!gone.has(3)) {
    // v10.1: "no 'i wanna' until after 0:40" — the first statement (bar 24
    // lands at 0:32 shipped) keeps only its dash; the pickup starts
    // singing from the first hook at 0:41 on. And the pickup itself is
    // the slowed take: "could be slowed a bit more, like faded … its
    // sorta rushed" (longdots.sh renders iwannaslow-* 1.5x with a soft
    // attack; faded = a shade lower, a shade wetter).
    if (at(bar) >= 54)
      sung(has("iwannaslow-c") ? "iwannaslow-c" : "iwanna-c-sung", t + 3.92 + jit(4), { gain: 0.72 * G, pan: 0.18, side: 0.5, dly: 0.24, atk: 0.05 });
    dashStack(t + 4.50, "d4", G * 0.98, bar);
  } else {
    // dropped: the bar becomes one long dash — v3's chorus4 move
    dashStack(t + 4.00, "d4", G * 0.90, bar);
  }

  // ── line 4 · "dot dot dash" ───────────────────────────────────────────
  if (!gone.has(4)) {
    // "the dot dot at 32 is too abrupt" — the staccato dots enter on a
    // 30 ms cosine instead of a cliff, a shade lower
    sung("dot-c-b3", t + 6.00 + jit(3), { gain: 0.74 * G, pan: -0.45, side: 0.75, dly: 0.36, atk: 0.03 });
    sung("dot-j-g3", t + 6.50 + jit(3), { gain: 0.70 * G, pan: 0.45, side: 0.75, dly: 0.36, atk: 0.03 });
    dashStack(t + 7.00, "b3", G * 0.92, bar);
  }

  // ── the answer figure — a different one each statement ────────────────
  if (answer === "dots") {
    sung("dot-c-b3", t + 1.30 + jit(4), { gain: 0.30 * G, pan: 0.38, side: 0.8, dly: 0.5, atk: 0.02 });
    sung(has("voxdot-c-b3") ? "voxdot-c-b3" : "dot-c-d4", t + 1.65 + jit(4),
      { gain: 0.26 * G, pan: -0.38, side: 0.8, dly: 0.5, atk: 0.03 });
  } else if (answer === "sos") {
    for (const [k, nm] of [[0, "dot-b3"], [1, "dot-fs3"], [2, "dot-d4"]])
      sung(nm, t + 1.20 + k * 0.25 + jit(3),
        { gain: 0.26 * G, pan: k === 1 ? 0 : k ? 0.4 : -0.4, side: 0.85, dly: 0.55 });
  }

  // a tag in the last bar: the double-time line as punctuation — or, while
  // the words are withheld, one quiet drifting dot closing the statement
  if (tag === "fast") {
    if (wordsIn(bar))
      for (const [k, oct] of [[0.00, "hi"], [0.50, "lo"]])
        sung(`runrealfast-fast-${oct}`, t + 7.0 + k + jit(3), {
          gain: (oct === "hi" ? 0.54 : 0.42) * G, pan: oct === "hi" ? 0.3 : -0.3,
          side: 0.7, dly: 0.45,
        });
    else
      dotDrift(t + 7.0 + jit(20), bar >> 2, { gain: 0.18 * G, pan: 0.30, dur: 0.95, stretch: 3.0, dly: 0.45, dark: 0.40 });
  }

  if (choirUnder) choir(bar, 0.12 * G);   // deeper, more inset
}

// ── THE SOS FIGURE ────────────────────────────────────────────────────
function sos(bar, g = 1) {
  const t = at(bar);
  for (const [k, n, p] of [[0, "dot-b3", -0.32], [1, "dot-fs3", 0.0], [2, "dot-d4", 0.32]])
    sung(n, t + k * BEAT + jit(3), { gain: 0.74 * g, pan: p, side: 0.72, dly: 0.30 });
  const w = wigDepth(bar) * 0.8;
  held("sos-dash-d4", t + 2.0 + jit(4), { gain: 0.70 * g, pan: -0.26, side: 0.62, dly: 0.20, wig: w, wigHz: 4.1, wigDrift: 0.3 * w });
  held("sos-dash-fs4", t + 3.5 + jit(4), { gain: 0.70 * g, pan: 0.26, side: 0.62, dly: 0.20, wig: w, wigHz: 5.5, wigPhase: 2.0, wigDrift: -0.3 * w });
  held("sos-dash-e4", t + 5.0 + jit(4), { gain: 0.68 * g, pan: 0.00, side: 0.62, dly: 0.20, wig: w, wigHz: 3.7, wigPhase: 4.0, wigDrift: 0.25 * w });
  for (const [k, n, p] of [[0, "dot-d4", 0.32], [1, "dot-a3", 0.0], [2, "dot-b3", -0.32]])
    sung(n, t + 6.5 + k * BEAT + jit(3), { gain: 0.64 * g, pan: p, side: 0.78, dly: 0.42 });
}

// The choir: sung "cult" held at three chord tones, 45 ms apart — which
// is itself a crossover, so it gets a small slow drift. v10.1: prefers
// the cultlong-* renders (5 s + 900 ms release, longdots.sh) so the chord
// decays under whatever comes next instead of stopping at four seconds —
// @jeffrey: "much longer decay … so they don't cut off when the perc and
// kicks come back".
const cultTake = (nm) => (has(`cultlong-${nm}`) ? `cultlong-${nm}` : `cult-${nm}`);
function choir(bar, g) {
  const t = at(bar);
  const picks = choirFor(degAt(bar));
  const gains = [0.46, 0.38, 0.30], pans = [0.0, -0.50, 0.50], sides = [0.40, 0.85, 0.85];
  const w = Math.min(7, wigDepth(bar) * 0.4);
  picks.forEach((nm, i) => sung(cultTake(nm), t + i * 0.045, {
    gain: g * (gains[i] ?? 0.28), pan: pans[i] ?? 0, side: sides[i] ?? 0.7, dark: 0.32,
    wig: w, wigHz: 0.7 + 0.25 * i, wigPhase: i * 2.3, wigDrift: (i % 2 ? -1 : 1) * w * 0.6, wigIn: 1.2,
  }));
  // v10.1: "downpitched / lower octaves … maybe get chordal" — the slow
  // harmonies grow a floor: the root take an octave below itself, dark
  // and centered, so the chord stands on something.
  sung(cultTake(picks[0]), t + 0.09, {
    gain: g * 0.55, semis: -12, dark: 0.52, side: 0.28, pan: 0,
    wig: w * 0.5, wigHz: 0.5, wigIn: 1.6, evVoice: "sub",
  });
}

// ACT IV is the perceptual reset: one Camille take at a time, never a chord
// stack. Four phrases cross the chord tones while the gravity impact bends
// their space. The rest of the ensemble has somewhere dramatic to return.
function secretCamille(bar, beat, gain, phase = 0) {
  const picks = choirFor(degAt(bar));
  const phrase = ((bar - S.secret[0]) / 2) | 0;
  const nm = picks[(phrase + 1) % picks.length];
  const take = has(`cult-${nm}`) ? `cult-${nm}` : cultTake(nm);
  const pans = [-0.18, 0.14, -0.06, 0.22];
  sung(take, at(bar, beat), {
    gain, pan: pans[phrase % pans.length], side: 0.58, dark: 0.24, dly: 0.46,
    atk: 0.16, wig: 7 + phrase * 1.5, wigHz: 0.44 + phrase * 0.07,
    wigPhase: phase + phrase * 1.7, wigDrift: phrase % 2 ? -4 : 4, wigIn: 0.9,
  });
}

// v10.1: @jeffrey — "can the distant blips and such change in their
// melody and counterpoint". The bops stop repeating one chord tone and
// walk a counterline: a per-bar degree chosen against the bass root in
// loose contrary motion, rotated a step every eight bars, so the phone's
// little answers form a tune of their own across the record.
const BLIP_LINE = [2, 4, 6, 4, 2, 0, 5, 3];
const blipMidi = (bar, k = 0) =>
  71 + sd(degAt(bar) + ((BLIP_LINE[(bar + k * 3) % 8] + (bar >> 3)) % 7));

// …and the phone knows the song: the four-line chorus melody as single
// sine beeps on the signal bus — "dance the 'original cult melody' with
// the phone, as a decoration. thatd be cute". Two bars, up an octave,
// swung wide across the field.
const PHONE_TUNE = [
  [0.00, 67], [0.50, 66], [1.00, 62],       // run real fast
  [2.00, 62], [2.50, 64], [3.00, 66],       // i wanna hide
  [4.00, 67], [4.75, 69],                   // a — waaay
  [6.00, 59], [6.50, 55], [7.00, 59],       // dot · dot · dash
];
// chorded now: each melody beep carries its in-scale third below on the
// second oscillator, and a detuned phase-shifted double answers 30 ms
// later on the far side.
const PHONE_HARM = [64, 62, 59, 59, 61, 62, 64, 66, 55, 52, 55];
function phoneTune(bar, g = 0.26) {
  PHONE_TUNE.forEach(([k, m], i) => {
    const tt = at(bar) + k * BEAT + jit(6);
    const gg = g * vel(0.2);
    beep(tt, hz(m + 12), hz(PHONE_HARM[i] + 12), 0.11,
      { gain: gg, pan: i % 2 ? 0.4 : -0.4, side: 0.85, dly: 0.45 });
    beep(tt + 0.030, hz(m + 12) * 1.004, hz(PHONE_HARM[i] + 12) * 0.9955, 0.11,
      { gain: gg * 0.55, pan: i % 2 ? -0.45 : 0.45, side: 0.95, dly: 0.60 });
  });
}

// v10.1: @jeffrey heard the reply's deep-wiggled dash line at 1:42 and
// called it "an awesome almost 'indian' sounding vocal — any way we can
// examine that technique / stretch it". What he heard was the crossover
// wiggle at depth 16 bending a held dash: vibrato wide enough to read as
// ornament — a gamaka, near enough. raga() leans in: the LONG dash take
// at a chord tone, wiggle twice as deep and slower, with a drift that
// slides it toward the next scale tone and back — the bend made melody.
const DASHLONG_PITCH = { b3: 59, d4: 62, e4: 64, fs4: 66 };
function raga(bar, beat, g = 0.42) {
  const pcs = new Set(triad(degAt(bar)).map((m) => ((m % 12) + 12) % 12));
  const ok = Object.keys(DASHLONG_PITCH)
    .filter((n) => has(`dashlong-camille-${n}`) && pcs.has(DASHLONG_PITCH[n] % 12));
  const nm = ok.length ? ok[bar % ok.length] : "b3";
  const w = Math.max(24, wigDepth(bar) * 1.8);
  held(`dashlong-camille-${nm}`, at(bar, beat) + jit(8), {
    gain: g, pan: bar % 2 ? 0.24 : -0.24, side: 0.7, dly: 0.30,
    wig: w, wigHz: 3.1, wigPhase: bar * 1.7, wigDrift: (bar % 2 ? -1 : 1) * w * 1.4, wigIn: 0.35,
  });
}

// v10.1: @jeffrey stopped at 1:37 — the reply statement's line-4 dots
// colliding with the stranger's sung F#4, a voxdot, and jeffrey's low
// dot-j-g3 — "whats the jeffrey vocal? can we try extending that
// crazyness for like 10 seconds or so?" So the collision becomes a
// passage: dotCloud() keeps the cluster alive — held stranger dots,
// camille/alex voxdots, and jeffrey's g3 answering every ~0.7–0.9 s,
// each wiggling at its own rate. Ten seconds of the chord being said
// as people.
function dotCloud(t0, dur = 10) {
  const picks = [
    ["altdot-71448-long", 0.26], ["voxdot-c-fs4", 0.22], ["dot-j-g3", 0.30],
    ["altdot-71437-long", 0.24], ["voxdot-a-d4", 0.22], ["dot-c-b3", 0.26],
    ["altdot-70551-long", 0.22], ["voxdot-c-b3", 0.22], ["dot-j-g3", 0.26],
    ["altdot-71441-long", 0.22], ["voxdot-a-a3", 0.20], ["dot-c-b3", 0.24],
  ];
  let u = 0;
  for (let k = 0; u < dur; k++) {
    const [nm, g] = picks[k % picks.length];
    if (has(nm)) sung(nm, t0 + u + jit(30), {
      gain: g * vel(0.2), pan: ((k * 5) % 7 - 3) / 4, side: 0.8, dark: 0.25, dly: 0.38,
      wig: 8 + (k % 3) * 4, wigHz: 0.4 + (k % 4) * 0.9, wigPhase: k * 1.7,
      wigDrift: (k % 2 ? -6 : 6), wigIn: 0.5,
    });
    u += 0.7 + ((k * 37) % 5) * 0.05;
  }
}

// v10.1: @jeffrey — "i want more camille dots … dots should be more
// extended usually … and i think some arpeggiated dot dot dot dot dot
// dot would be fun, like full scaled at times". Camille's sung dot bank
// happens to hold the entire B natural minor scale from B3 to A4 —
// seven takes, one per degree — so dotArp() literally runs the scale as
// syllables, up or down, lightly swung, softened attacks.
const CAM_SCALE = ["b3", "cs4", "d4", "e4", "fs4", "g4", "a4"];
function dotArp(t, { count = 5, up = true, gap = 0.16, gain = 0.34, pan = 0.2, dly = 0.34 } = {}) {
  for (let k = 0; k < count; k++) {
    const nm = `dot-c-${CAM_SCALE[up ? k % 7 : 6 - (k % 7)]}`;
    if (!has(nm)) continue;
    sung(nm, t + k * gap + jit(4) + (k % 2 ? 0.012 : 0), {
      gain: gain * (1 - 0.05 * k) * vel(0.15), pan: pan * (k % 2 ? -1 : 1),
      side: 0.7, dly, atk: 0.012,
    });
  }
}

// ── score ─────────────────────────────────────────────────────────────
console.log(`→ scoring ${BARS} bars @ ${BPM} BPM · B minor · ${Object.keys(S).length} acts · ${(BARS * BAR).toFixed(1)}s`);

// The first sound. From the watery-hole post (7071087615948148010): the
// metallic ding right after "guys" (source 1.64-2.09 s, spike at 1.71) —
// the line itself stays on the wall. Emitted so the spike lands exactly
// at the cut's first sample, ringing over the first kick. No jit() here:
// the parity RNG stream must not shift.
shot("waterhole", 8 * BAR - 0.12, { bus: "drums", gain: 0.95, pan: 0.0, side: 0.30, dly: 0.48 });
// The station ident — "whistlegraph dot org", jeffrey's voice snapped to
// B2/F#2/B1, on the tube bus so it clears the lyric hold: the record's
// first spoken words, so "dash" is never the first word.
shot("dotorg", 18 * BAR + 0.25, { bus: "tube", gain: 0.55, pan: 0.0, side: 0.40, dly: 0.30, dark: 0.25 });
// doooooot harmonies: the answer chord stretched five-fold into slow
// swells drifting through the intro — B minor sung glacially.
for (const [b, n, p] of [[12.5, "dot-b3", -0.35], [12.53, "dot-fs3", 0.35], [12.56, "dot-d4", 0.0],
                         [20.5, "dot-b3", 0.35], [20.53, "dot-fs3", -0.35], [20.56, "dot-d4", 0.0]])
  stretched(n, b * BAR, { gain: 0.10, pan: p, stretch: 3.5, dur: 4.0, side: 0.85, dark: 0.45, bus: "tube", dly: 0.55 });   // tamed
// The dot dot dots, seeded into the first 30 seconds — the v2 answer
// figure (b3 wide-left, fs3 wide-right, d4 ghost center) recessed on the
// tube bus so it reads as signal, growing with the intro. Fixed times —
// no rnd draws.
for (const [k, db] of [10, 14, 16, 22].entries()) {
  const u = db * BAR, g = 0.34 + 0.09 * k;
  shot("dot-b3", u + 3.0 * BEAT, { bus: "tube", gain: g, pan: -0.45, side: 0.75, dly: 0.45, dark: 0.20 });
  shot("dot-fs3", u + 3.5 * BEAT, { bus: "tube", gain: g, pan: 0.45, side: 0.75, dly: 0.45, dark: 0.20 });
  shot("dot-d4", u + 4.0 * BEAT, { bus: "tube", gain: g * 0.45, pan: 0.0, side: 0.60, dly: 0.60, dark: 0.25 });
}
// Camille's clearing reveals these colors one at a time: one accordion
// breath in its first half, one violin line in its second. No doubles.
shot("accordion-secret", 41 * BAR, { bus: "tube", gain: 0.22, pan: -0.08, side: 0.70, dly: 0.44, dark: 0.30 });
shot("violin-secret", 44.4 * BAR, { bus: "tube", gain: 0.24, pan: 0.10, side: 0.64, dly: 0.52, dark: 0.34 });
{
  const AN = ["accordion-b", "accordion-d", "accordion-g", "accordion-e"];
  // The bellows begin answering in IT SPREADS, then become a continuous
  // push/pull chord section in THE WHOLE MESSAGE. Each change has a broad
  // double and a quieter opposite-bellows answer one bar later.
  for (let ab = 64; ab < 68; ab += 2) {
    const name = AN[((ab - 64) / 2) % 4];
    shot(name, ab * BAR, { bus: "tube", gain: 0.12 * phraseLevel(ab, 0.8), pan: ((ab >> 1) & 1) ? -0.30 : 0.30,
      side: 0.72, dly: 0.36, dark: 0.28 });
  }
  for (let ab = 76; ab < 96; ab += 2) {
    const name = AN[((ab - 76) / 2) % 4];
    const pan = ((ab >> 1) & 1) ? -0.27 : 0.27;
    const breath = phraseLevel(ab, 0.8, 0.20);
    shot(name, ab * BAR, { bus: "tube", gain: 0.18 * breath, pan, side: 0.68, dly: 0.30, dark: 0.18 });
    shot(name, ab * BAR + 0.032, { bus: "tube", gain: 0.075 * breath, pan: -pan,
      side: 0.78, dly: 0.44, dark: 0.34, semis: 0.07 });
    shot(name, (ab + 1) * BAR, { bus: "tube", gain: 0.095 * phraseLevel(ab + 1, 0.8, 0.20), pan: -pan * 0.75,
      side: 0.72, dly: 0.38, dark: 0.26, off: 0.32 });
  }
}
// boing boing: a springy sproing on every act-VII chord change, pitched
// to the guitar wall it rides (B D G E). Fixed emits.
{
  const BN = ["boing-b", "boing-d", "boing-g", "boing-e"];
  for (let bb = 76; bb < 96; bb += 2)
    shot(BN[((bb - 76) / 2) % 4], bb * BAR, { bus: "tube", gain: 0.30, pan: ((bb >> 1) & 1) ? 0.32 : -0.32, side: 0.65, dly: 0.35, dark: 0.10 });
}
// The dark guitars (Peep pass): a palm-muted B-pedal chug through the
// reply and it-spreads, open ringing B-D-G-Em walls through the whole
// message. Music bus, fixed emits — no rnd draws.
// (complected: every guitar is a braided pair — a detuned double on the
// far side, later and darker; the whole section sits deeper.)
for (let gb = 48; gb < 68; gb += 4) {
  const phrase = phraseLevel(gb, 0.2, 0.22);
  const base = (0.12 + 0.0015 * (gb - 48)) * phrase;
  const trim = gb === 52 ? { dur: 4.0 } : {};
  shot("guitar-chug", gb * BAR, { bus: "music", gain: base, pan: -0.16, side: 0.5, dly: 0.30, dark: 0.34, ...trim });
  shot("guitar-chug", gb * BAR + 0.018, { bus: "music", gain: base * 0.52, pan: 0.30, side: 0.6, dly: 0.42, dark: 0.46, semis: 0.07, ...(gb === 52 ? { dur: 3.95 } : {}) });
}
for (let gb = 76; gb < 96; gb += 8) {
  const trim = gb + 8 > 96 ? { dur: (96 - gb) * BAR } : {};
  const phrase = phraseLevel(gb, 0.2, 0.22);
  shot("guitar-wide", gb * BAR, { bus: "music", gain: 0.19 * phrase, pan: 0.12, side: 0.6, dly: 0.40, dark: 0.32, ...trim });
  shot("guitar-wide", gb * BAR + 0.026, { bus: "music", gain: 0.11 * phrase, pan: -0.30, side: 0.7, dly: 0.52, dark: 0.44, semis: -0.08, ...trim });
}
{
  const GC = [
    [47, 54, 59, 62], // Bm: open fifth plus the minor third on top
    [50, 57, 62, 66], // D
    [43, 50, 55, 59], // G
    [40, 47, 52, 55], // Em
  ];
  for (let gb = 48; gb < 68; gb += 4)
    guitarChord(gb * BAR + 0.55 * BEAT, GC[((gb - 48) / 4) % 4], 3.25,
      (0.09 + 0.001 * (gb - 48)) * phraseLevel(gb, 0.2, 0.22), gb & 4 ? 0.22 : -0.22, !!(gb & 4));
  for (let gb = 76; gb < 96; gb += 2)
    guitarChord(gb * BAR + 0.12 * BEAT, GC[((gb - 76) / 2) % 4], 3.55,
      0.115 * phraseLevel(gb, 0.2, 0.22), gb & 2 ? 0.26 : -0.26, !!(gb & 2));

  const SHRED = [59, 62, 64, 66, 69, 71, 74, 76, 78];
  for (const [bar, flip] of [[55, true], [63, false], [83, false], [91, true]])
    guitarShred(at(bar, 2.75), flip ? [...SHRED].reverse() : SHRED,
      (bar === 55 ? 0.13 : bar >= 76 ? 0.135 : 0.115) * phraseLevel(bar, 0.2, 0.18), flip ? -0.42 : 0.42);
}

// One bowed answer crosses the middle pivot. It is exposed, not stacked,
// and hands the last note to the guitar run at the end of bar 55.
shot("violin-secret", 54.45 * BAR, {
  bus: "tube", gain: 0.17 * phraseLevel(54, 1.9), pan: 0.12,
  side: 0.68, dly: 0.48, dark: 0.31,
});


for (let bar = 0; bar < BARS; bar++) {
  const t = at(bar);
  const deg = degAt(bar);
  const root = bassRoot(deg);
  const chord = triad(deg);
  const four = bar % 4, eight = bar % 8;
  // the hands push and drag across a 32-bar breath; the kick stays the
  // clock (live-show pass)
  const push = 0.0045 * Math.sin(TAU * (bar % 32) / 32 + 0.7);
  const D = dense(bar);

  // ---- kick: 4/4, and it just stays. POW per hit, never a drop. --------
  if (kickOn(bar)) {
    const g = introBar(bar) ? 0.66 + 0.035 * (bar - 8)
      : inS(bar, "recognise") ? 0.90 - 0.06 * (bar - S.recognise[0]) : 0.96;
    for (let b = 0; b < 4; b++)
      kick(t + b * BEAT + jit(2.5), g * (b === 0 ? 1 : 0.95), { weight: b === 0 ? 1.0 : 0.86 });
  }

  // ---- hats ------------------------------------------------------------
  if (hatOn(bar)) {
    for (let s = 0; s < 8; s++) {
      const swing = s & 1 ? 0.035 : 0;
      const u = t + (s * 0.5 + swing) * BEAT + push + jit(5);
      if (s & 1) shot("hatC", u, { gain: 0.20 * vel(), pan: 0.20, side: 0.5, dur: 0.085 });
      else if (s % 4 === 2) shot("hatC", u, { gain: 0.11 * vel(), pan: -0.18, side: 0.5, dur: 0.065 });
    }
    if (D || eight >= 2)   // Peep pass: rolling hats most bars
      for (let s = 0; s < 16; s++)
        if (s % 4 === 1 || s % 4 === 3)
          shot("hatC", t + (s / 16) * BAR + push + jit(4), { gain: 0.055 * vel(0.5), pan: (s & 2 ? 0.35 : -0.35), side: 0.6, dur: 0.045 });
    if ((D || inS(bar, "spread")) && four === 3)
      shot("hatO", t + 3.5 * BEAT + 0.02, { gain: 0.20, pan: -0.28, side: 0.65, dur: 0.34 });
    if (four === 3 && kickOn(bar))   // Peep pass: the 32nd roll
      for (let r = 0; r < 6; r++)
        shot("hatC", t + 3.25 * BEAT + r * BEAT / 8,
          { gain: (0.034 + 0.015 * r) * vel(0.3), pan: 0.22 - 0.09 * r, side: 0.6, dur: 0.05 });
  }

  // ---- the pump's second trigger: rim on 3, clap when full -------------
  if (kickOn(bar)) {
    snare(t + 2 * BEAT + push + jit(5), vel());
    if (D) {
      shot("clap", t + 2 * BEAT + push + jit(5), { gain: 0.22, pan: -0.12, side: 0.72 });
      shot("snap", t + 2 * BEAT + 0.012, { gain: 0.09, pan: 0.30, side: 0.55 });
    }
  }
  if ((inS(bar, "spread") || inS(bar, "whole")) && !soloBar(bar) && !sparseSpreadBar(bar))
    for (let s = 0; s < 4; s++)
      shot("ride", t + (s + 0.5) * BEAT + jit(8), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.38 : -0.38, side: 0.7, dur: 0.22 });
  if (hatOn(bar) && !D)
    for (let s = 0; s < 4; s++)
      shot("tambo", t + (s + 0.5) * BEAT + jit(9), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.42 : -0.42, side: 0.7, dur: 0.09 });

  // ---- v10.1: "i want more perc play" — ghosts, blocks, a turnaround ---
  // The kit stops just keeping time and starts commenting on it: ghost
  // snares off the beat, a woodblock conversation through the dense acts,
  // and a climbing three-hit fill into every eighth bar.
  if (kickOn(bar)) {
    if (D || eight >= 4) {
      snare(t + 1.25 * BEAT + jit(6), 0.22 * vel(0.3));
      if (bar % 2) snare(t + 2.75 * BEAT + jit(6), 0.16 * vel(0.3));
    }
    if (D)
      for (const s of [0.75, 1.5, 3.25])
        if ((bar + s * 4) % 3 < 2)
          shot("block", t + s * BEAT + jit(8), { gain: 0.10 * vel(0.4), pan: s > 2 ? 0.5 : -0.34, side: 0.7, dur: 0.09 });
    if (eight === 7) {
      for (let k = 0; k < 3; k++)
        snare(t + (3 + k / 3) * BEAT + jit(4), (0.30 + 0.14 * k) * vel(0.2));
      shot("hatO", t + 3.9 * BEAT, { gain: 0.16, pan: 0.2, side: 0.6, dur: 0.3 });
    }
  }

  // ---- v10.1: the re-entries announce themselves — a reversed kick
  // rising into the downbeat, then two bars of wub under the groove.
  if (bar === S.reply[0] || bar === S.whole[0])
    revKick(t, 0.9, bar === S.whole[0] ? 0.55 : 0.48);
  if (bar === S.message[0] + 5)   // the sentence gets a door too
    revKick(t, 0.9, 0.48);
  if ((inS(bar, "reply") && bar - S.reply[0] < 2)
      || (!dotFieldBar(bar) && (bar === S.whole[0] - 2 || bar === S.whole[0] - 1)))
    wub(t, bassRoot(deg), 1, 0.26, 4);   // reply entrance only; the dot field stays free of sub ramps

  // ---- bass -------------------------------------------------------------
  if (kickOn(bar)) {
    const g = introBar(bar) ? 0.52 : inS(bar, "recognise") ? 0.72 : 0.86;
    for (let b = 0; b < 4; b++) {
      const fifth = four === 3 && b === 3;
      bass(t + (b + 0.5) * BEAT + jit(3), root + (fifth ? 7 : 0), 0.26, g, fifth ? root : null);
    }
    if (bar % 2 === 0) bass(t, root - 12, BAR * 0.90, g * 0.60);   // Peep pass: more sub
  }

  // ---- pad + stabs ------------------------------------------------------
  if (bar % 2 === 0 && !inS(bar, "secret") && !soloBar(bar) && !releaseGapBar(bar) && !dotFieldBar(bar))
    sines(t, chord.map((m) => m - 12), BAR * 1.85, inS(bar, "carrier") ? 0.055 + 0.012 * bar : 0.085,
      bar % 4 ? 0.22 : -0.22, 0.75, 0.5, 0, 0.30);
  if ((inS(bar, "message") || inS(bar, "reply") || inS(bar, "whole") || inS(bar, "spread"))
      && !soloBar(bar) && !sparseSpreadBar(bar))
    for (const b of [0.5, 2.5])
      sines(t + b * BEAT + jit(4), chord, 0.20, 0.075, b > 1 ? 0.36 : -0.36, 0.7, 0.9, 0.34);
  if (inS(bar, "spread") && !sparseSpreadBar(bar) && four === 1)
    sines(t + 3.5 * BEAT, chord.map((m) => m + 12), 0.13, 0.070, rnd() > 0.5 ? 0.5 : -0.5, 0.85, 0.7, 0.70);

  // ══ ACT I · CARRIER ══════════════════════════════════════════════════
  // v10: the record starts when the kicks start — cut-v10.sh trims the
  // master at bar 8, so this act survives only as the drone and dial tail
  // that ring under the first kick. Nothing worded is scheduled here, so
  // nothing worded can bleed across the cut.
  if (inS(bar, "carrier")) {
    if (bar % 2 === 0) {
      const digit = CULT_DIAL[(bar / 2) % 4];
      click(at(bar, 0.94), { gain: 0.34, pan: -0.5, side: 0.8, dly: 0.20 });
      dtmf(at(bar, 1), digit, 0.16, { gain: 0.62, pan: (bar / 2) % 2 ? 0.45 : -0.45, side: 0.85, dly: 0.34 });
    }
    if (bar % 4 === 3) tap(at(bar, 3.5), { gain: 0.30, pan: 0.3, side: 0.7, dly: 0.3 });
  }

  // ══ ACT II · THE MACHINE ALONE — AND A DOT DRIFTING THROUGH ══════════
  // v10.1 second pass. The first cut of this opening was a morse crowd —
  // two or three three-dot figures a bar, from every video at once — and
  // @jeffrey called it: "too chaotic … wayyyy less of them … all extended
  // a lot … more in the background". So now: sixteen bars of kick, ticking
  // and dial, with ONE dot at a time drifting behind them — each of the
  // nine harvested posts gets exactly one appearance across the opening,
  // its longest take stretched to a two-second sigh, dark, wide, deep in
  // the dial's echo. You still hear everyone before you hear anyone; you
  // just hear them the way you hear weather. The words stay withheld
  // until act VII.
  if (bar === 8 && has("phone-pickup-a"))
    // the first gesture on the shipped record: a hand picking up a real
    // receiver, right under the first kick
    shot("phone-pickup-a", at(8) + 0.02, { bus: "vox", gain: 0.20, pan: -0.12, side: 0.5, dark: 0.35, dur: 1.4, atk: 0.05, wig: 6, wigHz: 0.8, wigIn: 0.4 });
  if (bar === 12) phoneTune(bar, 0.20);   // the phone hums the song before anyone sings it
  if (introBar(bar) && bar % 2 === 0) {
    const k = (bar - 8) / 2;                       // videos 0–3, one per two bars
    dotDrift(at(bar, 2.0) + jit(30), k, {
      gain: 0.09 + 0.007 * k, pan: k % 2 ? 0.40 : -0.40,
      dur: BAR * 0.85, stretch: 4.5, dly: 0.52, dark: 0.62, deep: true,
    });
  }
  // one aesthetivoxed answer in the whole intro, under the bar-14 unison
  if (bar === 13)
    dotDriftVox(at(bar, 2.5) + jit(30), bar, { gain: 0.085, pan: 0.28, dur: BAR * 0.8, stretch: 4.0, dly: 0.5, dark: 0.55, semis: -12 });
  if (introBar(bar)) {
    if (bar % 2 === 0) {
      const digit = CULT_DIAL[(bar / 2) % 4];
      click(at(bar, 0.94), { gain: 0.30, pan: -0.5, side: 0.8, dly: 0.18 });
      dtmf(at(bar, 1), digit, 0.16, { gain: 0.54, pan: (bar / 2) % 2 ? 0.45 : -0.45, side: 0.85, dly: 0.30 });
    }
    // ticking: the machine is still on. Clicks on the "e" of every beat.
    for (let s = 0; s < 4; s++)
      if ((s + bar) % 2 === 0) click(t + (s + 0.25) * BEAT + jit(6), { gain: 0.70 * vel(0.4), pan: s & 1 ? 0.4 : -0.4, side: 0.7 });
    if (bar % 4 === 2) bop(at(bar, 3.5), hz(blipMidi(bar)), { gain: 0.30, pan: 0.35, side: 0.7, dly: 0.35 });
    // A hand on a surface while the voices arrive: one slow slide per
    // introduction, panned to the performer who is about to speak.
    if (bar % 2 === 0)
      frictionPath(at(bar, 0.5), BAR * 0.80, { shape: "slide", gain: 0.42,
        pan: bar === 8 ? 0.0 : bar === 10 ? 0.34 : -0.34, side: 0.26,
        path: "spiral", rough: 0.50 });
    // …and a drag INTO bar 14, where the three of them land together for
    // the first time. The gesture arrives with the unison.
    if (bar === 13)
      friction(at(13, 2.2), 1.7, { shape: "drag", gain: 0.62, pan: 0, side: 0.20,
        cut0: 759, cut1: 2300, res0: 103, res1: 214, rough: 0.62 });
  }
  if (sosBar(bar)) {
    // v10.1 second pass: the SOS bars keep the drift going — videos 4–7,
    // still one at a time, one per two bars, a shade closer than the intro
    // ones so the opening leans forward without ever crowding. The ninth
    // video speaks last, at bar 23, right before the first chorus: the
    // final stranger arriving as the loop begins. The machine's beeped SOS
    // at 20/22 stays the loudest signal in the act.
    const k = bar - 16;
    if (bar % 2 === 0)
      dotDrift(at(bar, 1.5) + jit(30), 4 + k / 2, {
        gain: 0.12, pan: (k / 2) % 2 ? -0.42 : 0.42,
        dur: BAR * 0.9, stretch: 4.2, dly: 0.48, dark: 0.58, deep: true,
      });
    if (bar === 19)
      dotDriftVox(at(bar, 2.5) + jit(30), bar, { gain: 0.09, pan: -0.26, dur: BAR * 0.8, stretch: 4.0, dly: 0.5, dark: 0.55, semis: -12 });
    if (bar === 23)
      dotDrift(at(bar, 2.0) + jit(30), 8, { gain: 0.13, pan: 0.0, dur: BAR * 0.9, stretch: 4.2, dly: 0.5, dark: 0.55, deep: true });
    if (bar === 20 || bar === 22) beepSOS(at(bar, 1), { gain: 0.50, pan: bar === 20 ? 0.55 : -0.55, dly: 0.40 });
    // taps mark where the dots will be, one bar ahead of the voices
    for (const s of [1.5, 3.5])
      tap(t + s * BEAT + jit(6), { gain: 0.68 * vel(0.3), pan: s > 2 ? 0.42 : -0.42, side: 0.75, dly: 0.30 });
    if (bar % 8 === 7) { click(at(bar, 3.0), { gain: 0.58, pan: 0, side: 0.6 }); click(at(bar, 3.25), { gain: 0.46, pan: 0.3, side: 0.6 }); }
    if (bar % 4 === 2) friction(at(bar, 1.0), 0.62, { shape: "skid", gain: 0.34,
      pan: -0.30, side: 0.24, cut0: 1900, cut1: 1000, res0: 239, res1: 149, rough: 0.58 });
  }

  // ══ ACT III · THE MESSAGE ════════════════════════════════════════════
  // v9: the chorus is the message. A full statement every eight bars over
  // the thick kick — the steady loop, now with the band — and the hook
  // drops to the off-slots as its answer.
  if (inS(bar, "message")) {
    const k = bar - S.message[0];
    if (k === 2) phoneTune(bar, 0.26);   // the precursor: the phone hums
                                         // the line into the door
    if (k === 0) chorus(bar, { lead: "both", answer: "dots" });
    if (k === 9) hook(bar, { full: true });   // first section: sentence order only
    if (k % 8 === 5) hook(bar, { full: false });   // shifted: sentence lands 0:42
  }
  if (inS(bar, "message")) {
    // the phone keeps time under the hook: a tap on the "and" of 4 and a
    // click pair in the hole the hook leaves at bar 3.
    tap(at(bar, 3.5) + jit(6), { gain: 0.62 * vel(0.3), pan: 0.40, side: 0.7, dly: 0.26 });
    if (four === 3) dotArp(at(bar, 2.0) + jit(8), { count: 5, up: (bar >> 2) % 2 === 0, gain: 0.30, pan: 0.30 });
    if (four === 3) {
      click(at(bar, 1.0), { gain: 0.50, pan: -0.42, side: 0.7, dly: 0.3 });
      bop(at(bar, 1.5), hz(blipMidi(bar, 1)), { gain: 0.26, pan: 0.42, side: 0.75, dly: 0.4 });
      // the drag that hands the next hook its downbeat
      friction(at(bar, 2.4), 1.6, { shape: "drag", gain: 0.46, pan: 0.12, side: 0.22,
        cut0: 819, cut1: 2200, res0: 109, res1: 203, rough: 0.58 });
    }
    if (four === 1)
      frictionPath(at(bar, 2.0), 0.50, { shape: "skid", gain: 0.30, pan: -0.36, side: 0.24,
        path: "edge", rough: 0.55 });
  }

  // ══ ACT IV · THE SECRET ══════════════════════════════════════════════
  // The turn. No kick. No beeps at all — the transmission stops, which is
  // the loudest thing in the record — except ONE click at the very top,
  // which is a receiver being picked up. v10.1: it IS one now — a real
  // handset lifted off a real cradle, with the synth click as its edge.
  if (bar === S.secret[0]) {
    click(at(bar, 0) - 0.06, { gain: 0.40, pan: 0, side: 0.35 });
    if (has("phone-pickup-b"))
      shot("phone-pickup-b", at(bar, 0) - 0.10, { bus: "vox", gain: 0.17, pan: 0.10, side: 0.45, dark: 0.40, dur: 1.2, atk: 0.04, wig: 6, wigHz: 0.7, wigIn: 0.4 });
  }
  if (inS(bar, "secret") && (bar - S.secret[0]) % 2 === 0) {
    const phrase = (bar - S.secret[0]) / 2;
    secretCamille(bar, 0.15, [0.46, 0.52, 0.56, 0.50][phrase], phrase * 0.7);
  }
  // One hand gesture and two distant low strings are the entire accompaniment.
  if (bar === S.secret[0] + 3)
    frictionPath(at(bar, 2.2), BAR * 0.72, { path: "spiralIn", shape: "slide",
      gain: 0.28, rough: 0.42, speed: 0.8, side: 0.24, dly: 0.16 });
  if (bar === S.secret[0] || bar === S.secret[0] + 4) {
    const gch = triad(degAt(bar));
    const phrase = (bar - S.secret[0]) / 4;
    guitar(at(bar, 0.65) + jit(6), gch[phrase ? 1 : 0] - 24,
      BAR * 2.6, phrase ? 0.14 : 0.16, phrase ? 0.20 : -0.14, 0.9974);
  }

  // ══ ACT V · THE REPLY ════════════════════════════════════════════════
  // v9: the loop comes back from the turn an octave down — the same words,
  // lower, like someone resuming a transmission they're no longer sure is
  // a joke — with SOS sung inside its holes; then the full-band statement.
  if (inS(bar, "reply") && !soloBar(bar)) {
    const k = bar - S.reply[0];
    if (k === 0) chorus(bar, { lead: "lo", g: 0.96, answer: "sos" });
    if (k === 8) chorus(bar, { lead: "both", answer: "dots" });
    if (k % 8 === 4) hook(bar, { full: k === 4 });
  }
  if (inS(bar, "reply") && !soloBar(bar) && four === 3)
    for (const [k, n, p] of [[0, "dot-d4", 0.34], [1, "dot-a3", -0.34]])
      sung(n, t + (2 + k) * BEAT + jit(4), { gain: 0.34, pan: p, side: 0.8, dly: 0.5 });
  // …and the answer itself: SOS, beeped, in the hole at the end of every
  // eight bars. The machine has started talking back.
  if (bar === S.reply[0] + 12) phoneTune(bar, 0.24);
  if (inS(bar, "reply") && !soloBar(bar) && eight === 7) {
    beepSOS(at(bar, 1), { gain: 0.52, pan: 0.55, dly: 0.45 });
    dotArp(at(bar, 2.5) + jit(8), { count: 7, up: false, gap: 0.18, gain: 0.26, pan: -0.25 });
  }
  if (inS(bar, "reply") && !soloBar(bar)) {
    for (const s of [0.75, 2.75]) tap(t + s * BEAT + jit(6), { gain: 0.38 * vel(0.3), pan: s > 2 ? -0.4 : 0.4, side: 0.7, dly: 0.24 });
    if (four === 1) click(at(bar, 3.75), { gain: 0.44, pan: 0.35, side: 0.7, dly: 0.35 });
    if (four === 3)
      friction(at(bar, 2.3), 1.7, { shape: "drag", gain: 0.44, pan: -0.14, side: 0.22,
        cut0: 859, cut1: 2400, res0: 114, res1: 223, rough: 0.60 });
    // the machine beeps, the hand answers: a skid straight off the SOS
    if (eight === 7)
      frictionPath(at(bar, 2.6), 0.75, { shape: "skid", gain: 0.42, pan: 0.44, side: 0.30,
        path: "spiralIn", rough: 0.66 });
  }
  if (inS(bar, "reply") && !soloBar(bar) && bar % 4 === 2) material(bar, "dash-camille-fs4-hold", 0.62, { grain: 0.055 });
  // A two-bar discovery window: A major opens under one bent Camille dash,
  // a bowed answer, and a guitar run. The rhythm section leaves completely.
  if (bar === 54) raga(bar, 0.60, 0.40);
  if (bar === 55) dotArp(at(bar, 0.35), {
    count: 5, up: false, gap: 0.22, gain: 0.18, pan: -0.28, dly: 0.48,
  });
  // the 1:42 ornament, featured: twice in the reply, twice where it spreads
  if (bar === S.reply[0] + 10 || bar === S.reply[0] + 14) raga(bar, 1.5, 0.44);
  if (bar === S.spread[0] + 3) raga(bar, 2.0, 0.38);

  // ══ ACT VI · IT SPREADS ══════════════════════════════════════════════
  // The held dashes become the whole lead, at the widest wiggle in the
  // record, and the beeps become a switchboard — a 3-against-4 pattern
  // of DTMF that never lines up with the bar.
  // v9: the loop does NOT stop here — it goes to ghosts. Two half-there
  // statements, lines missing, one voice each, while other people's
  // "cult"s answer from the edges. Steadiness through the strangest act
  // is the point: the message loops even while it comes apart.
  if (inS(bar, "spread") && !sparseSpreadBar(bar)) {
    const k = bar - S.spread[0];
    if (k === 0) chorus(bar, { lead: "hi", g: 0.55, drop: [2] });
    if (k === 8) chorus(bar, { lead: "lo", g: 0.60, drop: [2, 3] });
  }
  if (inS(bar, "spread") && !sparseSpreadBar(bar) && bar % 2 === 0) {
    const nm = ["fs4", "d4"][(bar / 2) % 2];
    const w = wigDepth(bar);
    held(`dash-camille-${nm}-hold`, at(bar, 1), {
      gain: 0.40, pan: -0.45, side: 0.85, dly: 0.30,
      wig: w, wigHz: 4.3, wigPhase: 0.4, wigDrift: +0.5 * w, wigIn: 0.5,
    });
    held(`dash-alex-${nm}-hold`, at(bar, 1.1), {
      gain: 0.37, pan: 0.45, side: 0.85, dly: 0.30,
      wig: w * 1.2, wigHz: 5.9, wigPhase: 2.6, wigDrift: -0.6 * w, wigIn: 0.45,
    });
    const low = { 0: "b2", 2: "a2", 3: "e2", 4: "b2", 5: "g2", 6: "a2" }[deg] ?? "b2";
    sung(`bassdash-${low}`, at(bar, 0), { gain: 0.26, pan: 0, side: 0.25, dark: 0.6 });
  }
  if (inS(bar, "spread") && !sparseSpreadBar(bar)) {
    const digits = ["2", "8", "5", "8", "*", "8", "5", "2"];
    for (let k = 0; k < 3; k++) {
      const u = t + (k * 4 / 3) * BEAT + jit(8);
      dtmf(u, digits[(bar * 3 + k) % digits.length], 0.055,
        { gain: 0.46 * vel(0.3), pan: k === 1 ? 0 : k ? 0.5 : -0.5, side: 0.85, dly: 0.40 });
    }
    if (four === 2) bop(at(bar, 3.25), hz(blipMidi(bar, 2)), { gain: 0.26, pan: -0.4, side: 0.8, dly: 0.45 });
    if (four === 1) dotArp(at(bar, 1.5) + jit(8), { count: 6, up: bar % 8 < 4, gap: 0.15, gain: 0.24, pan: 0.35, dly: 0.42 });
    // v10.1: somebody in the switchboard is on a ROTARY phone — one real
    // dial pass per half of the act, thrown to the edges like the cults
    if (bar === S.spread[0] + 4 && has("phone-rotary-a"))
      shot("phone-rotary-a", at(bar, 0.5), { bus: "vox", gain: 0.14, pan: -0.55, side: 0.85, dark: 0.45, dly: 0.45, atk: 0.08, wig: 9, wigHz: 0.35, wigIn: 0.6 });
    if (bar === S.spread[0] + 9 && has("phone-rotary-b"))
      shot("phone-rotary-b", at(bar, 1.0), { bus: "vox", gain: 0.13, pan: 0.55, side: 0.85, dark: 0.45, dly: 0.45, atk: 0.08, wig: 9, wigHz: 0.30, wigPhase: 2.1, wigIn: 0.6 });
    // it spreads: a skid every bar, alternating sides, never in the same
    // place twice — the friction equivalent of the wiggle coming apart.
    friction(at(bar, 2.5 + (bar % 3) * 0.25), 0.58, { shape: "skid",
      gain: 0.34 * vel(0.3), pan: bar % 2 ? 0.46 : -0.46, side: 0.30,
      cut0: 2200, cut1: 1050, res0: 280 - (bar % 4) * 30, res1: 149, rough: 0.62 });
    if (four === 0)
      friction(at(bar, 3.1), 0.95, { shape: "drag", gain: 0.30, pan: bar % 4 ? -0.2 : 0.2,
        side: 0.24, cut0: 900, cut1: 1900, res0: 119, res1: 189, rough: 0.55, synthetic: true });
  }
  // v10.1: @jeffrey — "around 1:57 the 'cult' should be extended alot, so
  // its more angelic from alex". Twice in the act (bars 66 and 70, 1:56
  // and 2:04 shipped) the top chord-tone take stretches to eight airy
  // seconds — bright, wide, mostly delay — with the take a third under it
  // drifting behind: an angel hanging over the switchboard while the raw
  // cults keep arriving beneath her.
  if (bar === S.spread[0] + 2) {
    const picks = choirFor(degAt(bar));
    const hi = picks[picks.length - 1], mid = picks[Math.max(0, picks.length - 2)];
    if (has(`cultlong-${hi}`)) {
      sung(`cultlong-${hi}`, at(bar, 1) + jit(10), {
        gain: 0.36, pan: 0.30, side: 0.92, dark: 0.05, dly: 0.55, atk: 0.35,
        wig: 7, wigHz: 0.4, wigPhase: 0.6, wigDrift: 4, wigIn: 1.2,
      });
      sung(`cultlong-${mid}`, at(bar, 1.5) + jit(10), {
        gain: 0.23, pan: -0.34, side: 0.92, dark: 0.12, dly: 0.55, atk: 0.45,
        wig: 8, wigHz: 0.31, wigPhase: 2.8, wigDrift: -5, wigIn: 1.0,
      });
    } else {
      stretched(`cult-${hi}`, at(bar, 1) + jit(10), {
        gain: 0.34, pan: 0.30, stretch: 2.3, dur: 8.0, side: 0.92, dark: 0.05, dly: 0.55,
      });
      stretched(`cult-${mid}`, at(bar, 1.5) + jit(10), {
        gain: 0.22, pan: -0.34, stretch: 2.6, dur: 8.0, side: 0.92, dark: 0.12, dly: 0.55,
      });
    }
  }

  // …and the word itself spreading: one other person's "cult" per bar,
  // from a different post each time, thrown to the edges. Six voices from
  // six videos across 2022, none of them the take the record is built from.
  if (inS(bar, "spread") && !sparseSpreadBar(bar)) {
    const [nm] = ALT_CULTS[(bar - S.spread[0]) % ALT_CULTS.length];
    if (has(nm))
      shot(nm, at(bar, 2.5 + ((bar % 3) * 0.25)), {
        bus: "vox", gain: 0.30 + 0.02 * ((bar - S.spread[0]) % 4),
        pan: bar % 2 ? 0.62 : -0.62, side: 0.90, dark: 0.16, dly: 0.48,
      });
  }
  // (v5's sos at spread+8 is gone — the ghost statement holds that slot)
  if (inS(bar, "spread") && !sparseSpreadBar(bar) && bar % 2 === 1) material(bar, "cult-d4", 0.72, { grain: 0.085, dly: 0.30 });

  // Four bars of dots occupying the whole field. No kit, bass, pad, held
  // dashes, accordion, or guitar: individual voices have mass, distance,
  // and room to finish. The last small descending scale points at the full
  // ensemble's bar-76 return without pre-filling it.
  if (bar === 72) dotCloud(at(bar, 0.18), BAR * 3.45);
  if (bar === 74) dotArp(at(bar, 1.15), {
    count: 7, up: false, gap: 0.24, gain: 0.15, pan: 0.24, dly: 0.55,
  });

  // ══ ACT VII · THE WHOLE MESSAGE ══════════════════════════════════════
  // Two full statements of the four-line chorus (bars 76 and 84), then the
  // hook once more at 92 as the come-down. Statement two doubles line 1 at
  // half length — the lyric performing itself — which is the only place in
  // the record anything is said twice as fast.
  // the full scale runs straight into the arrival: seven camille dots
  // climbing B3→A4 across the last bar before the words land
  if (bar === S.whole[0] - 1) dotArp(at(bar, 2.0), { count: 7, up: true, gap: 0.20, gain: 0.40, pan: 0.15, dly: 0.28 });
  if (bar === S.whole[0]) chorus(bar, { lead: "both", g: 1.0, answer: "dots", choirUnder: true });
  if (bar === S.whole[0] + 8) chorus(bar, { lead: "both", fast: true, g: 1.0, tag: "fast", choirUnder: true });
  if (bar === S.whole[0] + 16) hook(bar, { full: true });
  if (inS(bar, "whole")) {
    for (const s of [0.75, 1.75, 3.25]) tap(t + s * BEAT + jit(6), { gain: 0.52 * vel(0.4), pan: s > 2 ? -0.42 : 0.42, side: 0.7, dly: 0.22 });
    if (eight === 7) beepSOS(at(bar, 1), { gain: 0.34, pan: -0.55, dly: 0.5 });
    if (eight === 7)
      frictionPath(at(bar, 2.2), 1.8, { shape: "drag", gain: 0.52, pan: 0, side: 0.20,
        path: "scrub", rough: 0.64 });
    // off the back of "a — waaaay", which ends at +4.60 in each chorus
    if ((bar - S.whole[0]) % 8 === 2)
      friction(at(bar, 1.3), 0.66, { shape: "skid", gain: 0.36, pan: 0.40, side: 0.28,
        cut0: 2300, cut1: 1000, res0: 289, res1: 143, rough: 0.60 });
  }
  if (inS(bar, "whole") && bar % 4 === 3) material(bar, "cult-fs4", 0.66, { grain: 0.060, side: 0.9 });

  // ══ ACT VIII · RECOGNITION ═══════════════════════════════════════════
  // v9: the loop's last statement — one voice, half gain, the hide line
  // already gone — then the hook fragment, then people.
  if (bar === S.recognise[0]) chorus(bar, { lead: "hi", g: 0.62, drop: [2] });
  if (bar === S.recognise[0] + 4) hook(bar, { full: false });
  if (bar === S.recognise[0] + 6) phoneTune(bar, 0.22);
  if (inS(bar, "recognise") && bar % 2 === 0) material(bar, "cult-b3", 0.50, { grain: 0.10, dly: 0.34 });
  if (inS(bar, "recognise") && four === 3)
    friction(at(bar, 2.6), 1.4, { shape: "drag", side: 0.22,
      gain: 0.40 * (1 - (bar - S.recognise[0]) / 10), pan: 0.10,
      cut0: 819, cut1: 1900, res0: 109, res1: 174, rough: 0.52 });
  if (inS(bar, "recognise") && four === 1)
    click(at(bar, 2.5), { gain: 0.44 * (1 - (bar - S.recognise[0]) / 9), pan: -0.4, side: 0.7, dly: 0.35 });

  // ══ ACT IX · CARRIER OFF ═════════════════════════════════════════════
  // v10.1: THE HUMANS act is gone — the untreated recording with it — so
  // the record steps straight from the peel-off into the walk-out. Every
  // voice left on the record has been through the aesthetivox.
  // The last gesture: a hand leaving the surface, four bars long, starting
  // under the final drums and ending under the drone that is walking out.
  if (bar === S.carrieroff[0] - 2)
    frictionPath(at(bar, 1.0), BAR * 3.2, { shape: "slide", gain: 0.34, pan: -0.16, side: 0.22,
      path: "edgeBack", rough: 0.42, rel: 0.30 });
  if (inS(bar, "carrieroff")) {
    const k = bar - S.carrieroff[0];
    if (k % 2 === 0 && k <= 6)
      dtmf(at(bar, 1), CULT_DIAL[(k / 2) % 4], 0.14,
        { gain: 0.44 * (1 - k / 10), pan: k % 4 ? 0.5 : -0.5, side: 0.85, dly: 0.42 });
    if (k === 2) phoneTune(bar, 0.16);    // hums it once more on the way out
    if (k === 6) bop(at(bar, 3), hz(59), { gain: 0.24, pan: 0, side: 0.6, dly: 0.5 });
    // the last voice on the record is a stranger's dot, stretched to vapor
    if (k === 4)
      dotDrift(at(bar, 2) + jit(30), 8, { gain: 0.16, pan: 0, dur: BAR * 0.9, stretch: 4.5, dly: 0.55, dark: 0.45 });
  }

  // ---- the drone walking out ------------------------------------------
  if (inS(bar, "carrieroff") && bar % 2 === 0)
    choir(bar, 0.48 * (1 - (bar - S.carrieroff[0]) / 11));

  // one very soft noise wash at the act boundaries that need one — never a
  // riser, never resolving into anything.
  if ([S.three[0], S.message[0], S.reply[0], S.whole[0], S.carrieroff[0]].includes(bar))
    shot("sweep", t - 0.9, { gain: 0.055, pan: 0, bus: "music", side: 0.9, dur: 2.2, dark: 0.35 });
}

// The last sound on the record: the hang-up, after the music has already
// gone — and v10.1 makes it a REAL one, a handset meeting a cradle, with
// the two synth clicks the record has always ended on riding its edge.
// Then, very far away, one second of busy signal: the far end is still
// trying. The channel closes; the signal is still out there.
if (has("phone-hangup-a"))
  shot("phone-hangup-a", at(BARS) + 0.50, { bus: "vox", gain: 0.26, pan: 0, side: 0.35, dark: 0.20, atk: 0.012, wig: 5, wigHz: 0.9, wigIn: 0.3 });
click(at(BARS) + 0.55, { gain: 0.34, pan: 0, side: 0.30 });
click(at(BARS) + 0.62, { gain: 0.20, pan: 0.2, side: 0.30 });
if (has("phone-busy-us"))
  shot("phone-busy-us", at(BARS) + 1.35, { bus: "vox", gain: 0.06, pan: 0.30, side: 0.85, dark: 0.55, dly: 0.55, atk: 0.15, wig: 10, wigHz: 0.25, wigIn: 0.8 });

if (missing.size) console.warn(`  ! missing samples: ${[...missing].join(", ")}`);
console.log(`  ${kicks.length} kicks · ${snares.length} snares`);

// ── (2) THE TUBULAR COLOUR ────────────────────────────────────────────
// "pumping them with more tubular vibes".
//
// A tube is a delay line with feedback — that is not a metaphor, it is the
// physics — so the tubular colour here is a bank of four feedback combs
// tuned to B2 · B3 · F#4 · D5, which are the pitches the held dashes and
// the Bm harmony actually live on, damped at 2.4 kHz in the loop so the
// resonance is hollow rather than bright. Each comb is scaled by (1-fb) so
// it resonates at unity instead of screaming, and the bank is mixed in
// PARALLEL at 0.26, so the dry dash is still the thing you hear and the
// ring is what surrounds it.
//
// Then a low-drive asymmetric saturation (x + 0.11x²  through tanh at 1.35)
// with a DC blocker after it: the x² term adds the even harmonics that read
// as "tube" rather than "fuzz", and the DC blocker is what stops an
// asymmetric shaper from parking an offset on the bus and eating headroom.
{
  const FS = [123.47, 246.94, 369.99, 587.33];        // B2 · B3 · F#4 · D5
  // @jeffrey: "what is that sawtooth ... after the last dot, on the regular,
  // after the vocals". It was these. At fb .76-.84 the combs kept ringing
  // long after the voice that fed them stopped, and a comb ringing on its
  // own IS a harmonic series — which is what a sawtooth is. Measured: the
  // tube stem singing 492 Hz, the 4th harmonic of the B2 comb (123.47x4).
  // Halving the feedback keeps the hollow tube colour on the note but stops
  // it outliving the note; the heavier damping takes the top off the tail.
  const FB = [0.46, 0.50, 0.48, 0.42];
  const MIX = 0.30;
  const TUBE_COLOUR = 0;      // 0 = dry dashes, no combs, no saturation
  const damp = 1 - Math.exp((-TAU * 1500) / SR);
  const combs = FS.map((f, k) => ({
    d: Math.max(2, Math.round(SR / f)), fb: FB[k], g: (1 - FB[k]) * 1.15,
    bufL: new Float32Array(Math.round(SR / f) + 2), bufR: new Float32Array(Math.round(SR / f) + 2),
    iL: 0, iR: 0, zL: 0, zR: 0,
  }));
  const dcRc = 1 / (TAU * 18), dcA = dcRc / (dcRc + 1 / SR);
  let dcL = 0, dcR = 0, pL = 0, pR = 0;
  const SATT = Math.tanh(1.35);
  for (let i = 0; i < N; i++) {
    // The combs run on every sample, silence included — a tube keeps
    // ringing after the note that excited it has stopped.
    const xl = tubeL[i], xr = tubeR[i];
    let rl = 0, rr = 0;
    for (const c of combs) {
      const tl = c.bufL[c.iL], tr = c.bufR[c.iR];
      c.zL += damp * (tl - c.zL);
      c.zR += damp * (tr - c.zR);
      c.bufL[c.iL] = xl + c.zL * c.fb;
      c.bufR[c.iR] = xr + c.zR * c.fb;
      c.iL = c.iL + 1 >= c.d ? 0 : c.iL + 1;
      c.iR = c.iR + 1 >= c.d ? 0 : c.iR + 1;
      rl += tl * c.g; rr += tr * c.g;
    }
    // @jeffrey: "i hate the tube bus". The combs and the saturation are out —
    // the dashes pass through clean. The BUS stays, because the deep pump
    // keyed off kick and snare lives on it and that part he asked for and
    // liked. Set TUBE_COLOUR above 0 to bring the tube back.
    let l = xl + rl * MIX * TUBE_COLOUR, r = xr + rr * MIX * TUBE_COLOUR;
    if (TUBE_COLOUR > 0) {
      l = Math.tanh(1.35 * (l + 0.11 * l * l)) / SATT;
      r = Math.tanh(1.35 * (r + 0.11 * r * r)) / SATT;
    }
    dcL = dcA * (dcL + l - pL); pL = l;
    dcR = dcA * (dcR + r - pR); pR = r;
    tubeL[i] = dcL; tubeR[i] = dcR;
  }
}

// ── dub delay ─────────────────────────────────────────────────────────
{
  const D = Math.round(0.75 * BEAT * SR);       // dotted eighth = 0.375 s
  const FB = 0.42;
  const damp = 1 - Math.exp((-TAU * 2600) / SR);
  const hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1 / SR);
  const bL = new Float32Array(N + D + 1), bR = new Float32Array(N + D + 1);
  let dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
  for (let i = 0; i < N; i++) {
    const tapL = i >= D ? bL[i - D] : 0;
    const tapR = i >= D ? bR[i - D] : 0;
    dL += damp * (tapR - dL);
    dR += damp * (tapL - dR);
    bL[i] = dlySend[i] + dR * FB;
    bR[i] = dL * FB;
    hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
    hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
    musicL[i] += hpL * 0.50;
    musicR[i] += hpR * 0.50;
  }
}

// ── the two ducks ─────────────────────────────────────────────────────
// bedEnv:  kick only, depth 0.50 — v4's breath, unchanged.
// pumpEnv: kick AND snare, depth 0.72, faster recovery — the tubular pump.
//
// Before either envelope, the five buses become bodies in one elastic
// field. This ports the Special Signs principle (sound position IS the
// physical state): an impulse supplies velocity, Hooke's-law spring motion
// supplies the crossings and overshoot, damping settles it, distance becomes
// propagation delay/level, and the violent first excursion fractures a few
// milliseconds of the real bus into a repeating shard. The dry path remains
// present, so words keep their scored time while the room tears around them.
const ELASTIC_BODIES = [
  { name: "music", L: musicL, R: musicR, dir: -0.86, mass: 1.10, shard: 0.021 },
  { name: "drums", L: drumsL, R: drumsR, dir: 0.70, mass: 1.42, shard: 0.017 },
  { name: "vox",   L: voxL,   R: voxR,   dir: -0.42, mass: 0.74, shard: 0.026 },
  { name: "tube",  L: tubeL,  R: tubeR,  dir: 0.96, mass: 0.92, shard: 0.031 },
  { name: "sig",   L: sigL,   R: sigR,   dir: -0.98, mass: 0.58, shard: 0.013 },
];
const readLinear = (a, p) => {
  if (p <= 0) return a[0] || 0;
  const q = p | 0;
  if (q + 1 >= a.length) return a[a.length - 1] || 0;
  const f = p - q;
  return a[q] + (a[q + 1] - a[q]) * f;
};
function elasticizeBus(body, bodyIndex) {
  for (const ex of ELASTIC_EXPLOSIONS) {
    const start = Math.max(0, Math.round((ex.t - 0.08) * SR));
    const end = Math.min(N, Math.round((ex.t + ex.duration + 0.10) * SR));
    const srcL = body.L.slice(start, end), srcR = body.R.slice(start, end);
    const anchor = Math.max(0, Math.round((ex.t + 0.035 + bodyIndex * 0.009) * SR) - start);
    const grain = Math.max(16, Math.round(body.shard * SR));
    for (let i = Math.max(start, Math.round(ex.t * SR)); i < end; i++) {
      const age = i / SR - ex.t;
      if (age < 0 || age > ex.duration) continue;
      const tail = smooth(clamp((ex.duration - age) / 0.48, 0, 1));
      const spring = ex.strength / body.mass * Math.exp(-ex.damping * age)
        * Math.sin(TAU * ex.hz * age) * tail;
      const travel = Math.abs(spring);
      const delay = travel * (0.007 + 0.012 / body.mass) * SR;
      const local = i - start;
      const pos = Math.max(0, local - delay);
      let ml = readLinear(srcL, pos), mr = readLinear(srcR, pos);

      // Distance darkens/attenuates; signed displacement is azimuth. The
      // variable propagation delay is also the Doppler bend on each return.
      const distance = 1 / (1 + 0.24 * travel);
      const pan = clamp(body.dir * spring * 0.86, -0.92, 0.92);
      ml *= Math.sqrt(1 - pan) * distance;
      mr *= Math.sqrt(1 + pan) * distance;

      // At the impact only, hold and repeat a tiny piece of the actual bus.
      // Different body grains refuse to line up: the screwiness is temporal,
      // while the spring remains one coherent gravitational gesture.
      const fracture = ex.glitch * Math.exp(-2.7 * age)
        * smooth(clamp(age / 0.022, 0, 1)) * tail;
      if (fracture > 0.012) {
        const gp = Math.min(srcL.length - 2, anchor + ((local - anchor + grain * 64) % grain));
        const gl = readLinear(srcL, gp), gr = readLinear(srcR, gp);
        const swap = bodyIndex & 1;
        ml = ml * (1 - 0.34 * fracture) + (swap ? gr : gl) * 0.34 * fracture;
        mr = mr * (1 - 0.34 * fracture) + (swap ? gl : gr) * 0.34 * fracture;
      }

      const wet = clamp(0.46 + 0.34 * travel + 0.16 * fracture, 0, 0.88);
      body.L[i] = srcL[local] * (1 - wet) + ml * wet;
      body.R[i] = srcR[local] * (1 - wet) + mr * wet;
    }
  }
}
for (let k = 0; k < ELASTIC_BODIES.length; k++) elasticizeBus(ELASTIC_BODIES[k], k);

const bedEnv = buildEnv(kicks.map((t) => ({ t, depth: 0.50, atk: 0.009, rel: 0.31 })));
const pumpEnv = buildEnv([
  ...kicks.map((t) => ({ t, depth: 0.72, atk: 0.009, rel: 0.26 })),
  ...snares.map((t) => ({ t, depth: 0.44, atk: 0.009, rel: 0.20 })),
]);

// ── stems ─────────────────────────────────────────────────────────────
const writeStereo = (path, L, R) => {
  const bytes = N * 2 * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0, "ascii"); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8, "ascii");
  buf.write("fmt ", 12, "ascii"); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28);
  buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36, "ascii"); buf.writeUInt32LE(bytes, 40);
  for (let i = 0; i < N; i++) {
    buf.writeFloatLE(L[i], 44 + i * 8);
    buf.writeFloatLE(R[i], 44 + i * 8 + 4);
  }
  writeFileSync(path, buf);
};
if (process.argv.includes("--stems")) {
  const dir = resolve(OUT, "stems");
  mkdirSync(dir, { recursive: true });
  // post-duck, post-makeup: this is what each bus actually contributes, so
  // "is the lead proud of the bed" is answerable by measuring these.
  const mk = (L, R, env, g) => {
    const a = new Float32Array(N), b = new Float32Array(N);
    for (let i = 0; i < N; i++) { a[i] = L[i] * (env ? env[i] : 1) * g; b[i] = R[i] * (env ? env[i] : 1) * g; }
    return [a, b];
  };
  const voxDuck = new Float32Array(N);
  const sigDuck = new Float32Array(N);
  for (let i = 0; i < N; i++) { voxDuck[i] = Math.pow(bedEnv[i], 0.25); sigDuck[i] = Math.pow(bedEnv[i], 0.5); }
  writeStereo(resolve(dir, "v10-vox.wav"), ...mk(voxL, voxR, voxDuck, VOXG));
  writeStereo(resolve(dir, "v10-tube.wav"), ...mk(tubeL, tubeR, pumpEnv, TUBEG));
  writeStereo(resolve(dir, "v10-music.wav"), ...mk(musicL, musicR, bedEnv, 1));
  writeStereo(resolve(dir, "v10-drums.wav"), ...mk(drumsL, drumsR, null, 1));
  writeStereo(resolve(dir, "v10-signal.wav"), ...mk(sigL, sigR, sigDuck, SIGG));
  console.log(`  stems → ${dir}`);
}

// ── Special Sign side return ──────────────────────────────────────────
// Band-limited 80 Hz – 11.5 kHz, handed back antisymmetrically (L=+, R=−)
// on a slewed send that breathes with the arrangement. Each bus's share of
// the side field carries that bus's duck, so the tube's side content pumps
// exactly the way its direct signal does. Mono-safe by construction.
const sideOut = new Float32Array(N);
{
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = 0.9;
  for (let i = 0; i < N; i++) {
    const be = bedEnv[i], pe = pumpEnv[i];
    const s = sideB[i] * be + sideV[i] * Math.pow(be, 0.25) + sideT[i] * pe * TUBEG + sideS[i] * Math.pow(be, 0.5);
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = (i / SR) / BAR;
    const target =
      bar < S.three[0] ? 0.92 :
      bar < S.message[0] ? 0.70 :
      bar < S.secret[0] ? 0.56 :
      bar < S.reply[0] ? 0.90 :
      bar < S.spread[0] ? 0.52 :
      bar < S.whole[0] ? 0.86 :
      bar < S.recognise[0] ? 0.46 :
      bar < S.carrieroff[0] ? 0.60 : 0.90;
    send += 0.00004 * (target - send);
    sideOut[i] = lp * send;
  }
}

// ── sum ───────────────────────────────────────────────────────────────
// Clean: duck, fade, measure, ONE linear trim. No master tanh anywhere.
let peak = 0;
const L = new Float32Array(N), R = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const be = bedEnv[i], pe = pumpEnv[i];
  const dv = Math.pow(be, 0.25);            // voices only breathe
  const ds = Math.pow(be, 0.5);             // the signal layer sits in it
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.6 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  const l = (musicL[i] * be + voxL[i] * dv * VOXG + tubeL[i] * pe * TUBEG + sigL[i] * ds * SIGG
    + drumsL[i] + sideOut[i]) * fade;
  const r = (musicR[i] * be + voxR[i] * dv * VOXG + tubeR[i] * pe * TUBEG + sigR[i] * ds * SIGG
    + drumsR[i] - sideOut[i]) * fade;
  // the listener moves: slow stereo rotation at two incommensurate rates,
  // breathing nearer and farther (live-show pass)
  const lt = i / SR;
  const th = 0.10 * Math.sin(TAU * 0.021 * lt) + 0.05 * Math.sin(TAU * 0.009 * lt + 1.3);
  const br = 1 + 0.035 * Math.sin(TAU * 0.016 * lt + 0.5);
  const lr = (l * Math.cos(th) - r * Math.sin(th)) * br;
  const rr = (l * Math.sin(th) + r * Math.cos(th)) * br;
  L[i] = lr; R[i] = rr;
  if (Math.abs(lr) > peak) peak = Math.abs(lr);
  if (Math.abs(rr) > peak) peak = Math.abs(rr);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= norm; R[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)}`);

const outWav = resolve(OUT, "cult-remix-v10-full.wav");
writeStereo(outWav, L, R);

const usedAlt = [...new Set(EVENTS.map((e) => e.sample).filter((k) => k && k.startsWith("alt-")))];
EVENTS.sort((a, b) => a.t - b.t || (a.bus < b.bus ? -1 : 1));
const voiceCounts = {};
for (const e of EVENTS) voiceCounts[e.voice] = (voiceCounts[e.voice] ?? 0) + 1;
const clock = (sec) => `${Math.floor(sec / 60)}:${String(Math.floor(sec % 60)).padStart(2, "0")}`;
console.log(`  ${EVENTS.length} events · ${Object.entries(voiceCounts).map(([k, v]) => `${k} ${v}`).join(" · ")}`);
writeFileSync(resolve(OUT, "cult-remix-v10.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph cult --- remix (v10, kicks first)",
  renderer: "pop/cult/bin/render10.mjs",
  narrative: {
    logline: "A signal goes out, and it is answered, and the joke turns out to be true — "
      + "and the message never stops looping while it happens.",
    premise: "The source is eight seconds of three friends chanting morse at a camera, captioned "
      + "\"dot dot dot dash dash dash dot dot dot (jk)\" — SOS, disclaimed. The arrangement takes the "
      + "disclaimer away over four minutes.",
    turn: "1:20 — act IV. The kick stops, the beeps stop, one click is a receiver being picked up, "
      + "and the word arrives on its own, harmonised: CULT. Everything before it is transmission; "
      + "everything after it is confession.",
    learning: [
      "0:00 the message is already looping — one voice, no drums — while DTMF dials C-U-L-T (2-8-5-8)",
      "0:16 there are three people (one register at a time: B2, F#3, C#4)",
      "0:28 they can land on one syllable, and they beat before they lock",
      "0:32 the signal is SOS",
      "0:48 the loop gets the band: the statement over the thick kick",
      "1:20 the word is 'cult'",
      "1:36 something answers, in beeps",
      "2:08 it has spread and it no longer agrees about pitch",
      "2:32 the complete four-line message, and the harmony comes home",
      "3:28 it was three friends and one take all along",
      "3:44 the channel closes; the last sound is a hang-up, after the music",
    ],
    acts: Object.fromEntries(Object.entries(S).map(([k, [a, b]]) => [k, {
      act: ACTS[k], bars: [a, b],
      seconds: [+at(a).toFixed(2), +at(b).toFixed(2)],
      clock: [`${Math.floor(at(a) / 60)}:${String(Math.round(at(a) % 60)).padStart(2, "0")}`,
        `${Math.floor(at(b) / 60)}:${String(Math.round(at(b) % 60)).padStart(2, "0")}`],
    }])),
  },
  v5Changes: {
    "1-crossoverWiggle": "Per-performer vibrato + slow detune drift applied ONLY where held voices "
      + "overlap (dashStack's three-people-one-syllable, the spread section's stacked pads, the "
      + "'a—waaaay' hold, the choir's 45 ms-offset cults). Depth ramps in over the first third of "
      + "each note so attacks stay in tune, and grows with the narrative: 4 cents when the three "
      + "first meet, 22 where it spreads.",
    "2-tubularPump": "The held dashes have their own bus: four feedback combs tuned B2·B3·F#4·D5 "
      + "damped at 2.4 kHz (a tube is a delay line with feedback), scaled by (1-fb) so they resonate "
      + "at unity, mixed in parallel at 0.34; then asymmetric tube saturation (x + 0.11x² through "
      + "tanh 1.35) with a DC blocker; then a deep pump — depth 0.72 keyed off kick AND snare, "
      + "against the bed's 0.50.",
    "3-thickKick": "200→47 Hz sweep at 62/s, two-stage envelope (34/s slam over a 7/s tail), tanh "
      + "saturation on the kick VOICE at drive 2.4, a real transient (1.6 k + 3.9 kHz blips) and a "
      + "separate 44 Hz sub layer. Weight and impact per hit — no drop, no riser, no crash.",
    "4-signalLayer": "A fifth bus of real DTMF pairs, bops, 4 ms clicks and woody taps, ducked at "
      + "half depth. Dials C-U-L-T in act I, ticks under II–III, GOES SILENT in IV, beeps SOS back "
      + "in V, becomes a 3-against-4 switchboard in VI, and has the last sound on the record.",
    "6-trackdrumSkids": "A direct port of the Continuous membrane friction block in "
      + "slab/menuband/Sources/MenuBand/MenuBandPercussion.swift (~1296–1338): two low-pass states "
      + "over the same white sample with the friction signal as their DIFFERENCE, a sine carrier "
      + "self-modulated by that band (1 + tanh(band*8)*0.055), tanh grip, and a level target through "
      + "separate attack/release one-poles so the gesture lives in the envelope. v5 sweeps cutoff and "
      + "carrier across each gesture. 49 of them: drag (into a downbeat), skid (off the back of a hit), "
      + "slide (under a transition). Act IV is the showcase — with the beeps silent, a hand on a "
      + "drumhead is the only thing still moving.",
    "7-otherTikToks": "posts.json tags TWELVE posts with the `cult` work, not one. The other eleven "
      + "were pulled from the assets mirror and chopped into alt/samples/ — 81 excerpts including ten "
      + "fresh utterances of the word, and (checked with librosa.pyin) no whistles at all. Six of the "
      + "ten play in act VI, one per bar, from the edges; the sung one and a sung 'the three of us are "
      + "in a' go to act IX; act X's last word is somebody else's. Receipt: alt/harvest.json.",
    "5-narrative": "See narrative.acts — ten acts, and every element's entry and exit serves one.",
  },
  source: {
    work: "cult — The Three of Us Are in a Cult (Whistlegraph, 2022)",
    tiktok: "https://www.tiktok.com/@whistlegraph/video/7055106286232325423",
    mirror: "https://assets.aesthetic.computer/whistlegraph/index/posts/7055106286232325423.mp4",
    performers: ["Jeffrey Alan Scudder", "Camille Klein", "Alex Freundlich"],
    transcript: "Dash, dash, dash, dot, dot, dot. The three of us are in a cult.",
    caption: "dot dot dot dash dash dash dot dot dot (jk)",
    measuredRegisters: { jeffrey: "B2 ≈127 Hz", alex: "F#3 ≈193 Hz", camille: "C#4 ≈245 Hz", cult: "B3 ≈247 Hz" },
    altTakesUsed: usedAlt,
  },
  harmony: {
    key: "B natural minor",
    source: "PROGRESSIONS_CHILL, recap/bin/trance.mjs — the 8-row pool in pop/dance/cutezenwaltzi.md",
    rows: ROWS, home: HOME,
    homeNote: "Act VII pins the harmony to Bm·D·G·Em — the only place the progression stops wandering, "
      + "and it happens exactly where the complete lyric arrives.",
    chordBars: 2, cycleBars: 32,
  },
  spatialExplosions: ELASTIC_EXPLOSIONS.map((e) => ({
    t: e.t, bar: e.bar, kind: e.kind, duration: e.duration, strength: e.strength,
    springHz: e.hz, damping: e.damping, glitch: e.glitch,
  })),
  tempoBPM: BPM, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  // Absolute-seconds section table, with the narrative line the video can
  // caption each act with.
  sections: Object.entries(S).map(([k, [a, b]]) => ({
    key: k, act: ACTS[k], bars: [a, b],
    start: +at(a).toFixed(3), end: +at(b).toFixed(3),
    clock: [clock(at(a)), clock(at(b))],
  })),
  buses: {
    music: "harmonic bed, pad, stabs, bass, dub-delay return — ducks with the kick (depth 0.50)",
    drums: "kick, hats, perc AND the TrackDrum friction skids — never ducks",
    vox: "sung words, dots, choir, material grains — light duck (bedEnv^0.25), +3 dB makeup",
    tube: "the held dashes — comb-resonated, tube-saturated, deep kick+snare pump (depth 0.72)",
    sig: "beeps, bops, clicks, taps — half-depth duck (bedEnv^0.5)",
  },
  eventSchema: {
    t: "absolute seconds", voice: Object.keys(voiceCounts).sort(),
    who: "camille | alex | jeffrey, on every performer-specific vocal hit",
    fields: "dur, gain, pan, bus, midi/midis/hz, sample, wiggleCents, shape/cut/res (skids), digit (DTMF)",
  },
  eventCounts: voiceCounts,
  events: EVENTS,
  hook: "dash — i wanna — dash — i wanna — run REAL fast — dot dot (four bars, sung)",
  chorus: ["run real fast", "i wanna hide away", "i wanna dash", "dot dot dash"],
  sosFigure: "··· ——— ··· sung as a four-bar riff in act II, beeped back in DTMF in acts V and VII",
  prePeak: +peak.toFixed(6), linearTrim: +norm.toFixed(4),
  kicks: kicks.length, snares: snares.length,
}, null, 2));

console.log(`✓ ${outWav}  (${(BARS * BAR).toFixed(1)}s)`);
