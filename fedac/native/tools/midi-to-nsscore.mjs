#!/usr/bin/env node
// midi-to-nsscore.mjs — a generic MIDI spatializer for Note(s)pat(ial) Native.
//
// Any Standard MIDI File becomes a ring .nsscore the laptops perform through
// pieces/spatial-rehearsal.mjs: every note keeps its time, pitch, velocity
// and GM program (the native gm_synth voices it; `wave` is the fallback on a
// build without that program), and gains a place in the room. The room is
// the same as the house scores: `seats` laptops, `ring` of them on the
// circle, one optionally held at the center.
//
// Placement (--place):
//   octave    one octave is one lap of the ring: a note's pitch class sets
//             its azimuth, so scales run around the room and chords spread
//             across it. Twelve pinned lanes per source. (default)
//   register  a source's whole range sweeps the ring once, low at the front
//             and rising clockwise; sixteen pinned lanes per source.
//   tracks    one lane per source, spaced evenly around the ring; --orbit S
//             makes them all wander at S seconds a lap, alternating ways.
//   orbit     one lane per source, each on its own slow orbit.
// Channel 10 is a drum kit: kick to the front, snare to the rear, hats on
// a five-second orbit, cymbals across every seat, toms by pitch. The
// recipes are compose-notespatial-native's, so a laptop passes them.
// --held <regex> sends matching sources to the center laptop (needs --center).
//
//   node fedac/native/tools/midi-to-nsscore.mjs song.mid [--name slug]
//        [--seats 6] [--center 5|none] [--place octave|register|tracks|orbit]
//        [--orbit S] [--held regex] [--gain .36] [--max-dur 2] [--rate 1]
//        [--transpose 0] [--min-vel 1] [--out path.nsscore] [--title "..."]
//
// Then, as with any ring score:
//   node fedac/native/tools/notespatial-native-check.mjs scores/<slug>.nsscore
//   node fedac/native/tools/spatial-rehearsal.mjs deploy --score <slug> HOST...
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { basename, resolve } from 'node:path';
import { GM_NAMES } from './notespatial-orchestra.mjs';
import { voicePosition, sourceGain, noteColor } from '../lib/spatial-rehearsal.mjs';

const TAU = Math.PI * 2;
const r4 = x => +x.toFixed(4);
const args = process.argv.slice(2);
const file = args.find((a, i) => !a.startsWith('--') && (i === 0 || !args[i - 1].startsWith('--'))); // every option takes a value
const opt = (name, fallback) => { const i = args.indexOf(`--${name}`); return i >= 0 && i + 1 < args.length ? args[i + 1] : fallback; };
if (!file) { console.error('usage: midi-to-nsscore.mjs song.mid [--name slug] [--seats 6] [--center 5|none] [--place octave|register|tracks|orbit] ...'); process.exit(1); }

const SEATS = +opt('seats', 6);
const centerOpt = opt('center', SEATS === 6 ? '5' : 'none');
const CENTER = centerOpt === 'none' ? null : +centerOpt;
const RING = CENTER === null ? SEATS : SEATS - 1;
const PLACE = opt('place', 'octave');
const ORBIT = +opt('orbit', 0);
const HELD = opt('held', null) ? new RegExp(opt('held'), 'i') : null;
const GAIN = +opt('gain', .36);
const MAX_DUR = +opt('max-dur', 2);
const RATE = +opt('rate', 1);
const TRANSPOSE = +opt('transpose', 0);
const MIN_VEL = +opt('min-vel', 1);
if (!['octave', 'register', 'tracks', 'orbit'].includes(PLACE)) throw Error(`unknown --place ${PLACE}`);
if (HELD && CENTER === null) throw Error('--held needs a --center seat');

// ── Standard MIDI File ────────────────────────────────────────────────
function parseMidi(buf) {
  let p = 0;
  const u32 = () => (buf[p++] << 24 | buf[p++] << 16 | buf[p++] << 8 | buf[p++]) >>> 0;
  const u16 = () => buf[p++] << 8 | buf[p++];
  const str = n => { const s = buf.subarray(p, p + n).toString('latin1'); p += n; return s; };
  const vlq = () => { let v = 0, c; do { c = buf[p++]; v = (v << 7) | (c & 0x7f); } while (c & 0x80); return v >>> 0; };
  if (str(4) !== 'MThd') throw Error('not a Standard MIDI File');
  const hlen = u32(); const format = u16(), ntracks = u16(), division = u16(); p += hlen - 6;
  if (division & 0x8000) throw Error('SMPTE time division is not supported');
  const tempos = [], markers = [], programs = [], tracks = [];
  let title = null;
  for (let ti = 0; ti < ntracks; ti++) {
    if (str(4) !== 'MTrk') throw Error(`track ${ti}: no MTrk`);
    const len = u32(), end = p + len;
    let tick = 0, status = 0, name = null;
    const notes = [], open = new Map();
    while (p < end) {
      tick += vlq();
      let s = buf[p];
      if (s & 0x80) { p++; status = s; } else s = status;
      if (s === 0xff) {
        const type = buf[p++], len = vlq(), data = buf.subarray(p, p + len); p += len;
        if (type === 0x03) name = data.toString('latin1').trim();
        else if (type === 0x01 && ti === 0 && !title) title = data.toString('latin1').trim();
        else if (type === 0x06) markers.push({ tick, name: data.toString('latin1').trim() });
        else if (type === 0x51) tempos.push({ tick, us: data[0] << 16 | data[1] << 8 | data[2] });
      } else if (s === 0xf0 || s === 0xf7) { p += vlq(); }
      else {
        const hi = s & 0xf0, ch = s & 0x0f;
        if (hi === 0xc0) programs.push({ tick, ch, program: buf[p++] });
        else if (hi === 0xd0) p++;
        else {
          const d1 = buf[p++], d2 = buf[p++];
          const key = ch * 128 + d1;
          if (hi === 0x90 && d2 > 0) { if (!open.has(key)) open.set(key, []); open.get(key).push({ tick, vel: d2 }); }
          else if (hi === 0x80 || hi === 0x90) {
            const q = open.get(key)?.shift();
            if (q) notes.push({ tick: q.tick, end: tick, midi: d1, vel: q.vel, ch, track: ti });
          }
        }
      }
    }
    for (const [key, list] of open) for (const q of list) notes.push({ tick: q.tick, end: tick, midi: key % 128, vel: q.vel, ch: Math.floor(key / 128), track: ti }); // notes never released end with the track
    p = end;
    tracks.push({ index: ti, name: name || (ti === 0 && title ? title : null), notes });
  }
  if (!tempos.length) tempos.push({ tick: 0, us: 500000 });
  tempos.sort((a, b) => a.tick - b.tick);
  if (tempos[0].tick > 0) tempos.unshift({ tick: 0, us: 500000 });
  programs.sort((a, b) => a.tick - b.tick);
  // tick → seconds through the tempo map
  let sec = 0;
  for (let i = 0; i < tempos.length; i++) { tempos[i].sec = sec; if (i + 1 < tempos.length) sec += (tempos[i + 1].tick - tempos[i].tick) * tempos[i].us / 1e6 / division; }
  const seconds = tick => { let seg = tempos[0]; for (const t of tempos) { if (t.tick > tick) break; seg = t; } return seg.sec + (tick - seg.tick) * seg.us / 1e6 / division; };
  const programAt = (ch, tick) => { let prog = 0; for (const c of programs) { if (c.tick > tick) break; if (c.ch === ch) prog = c.program; } return prog; };
  return { format, division, title, tempos, markers, tracks, seconds, programAt };
}

const midi = parseMidi(await readFile(file));
const hz = m => +(440 * Math.pow(2, (m - 69) / 12)).toFixed(2);
const NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const noteName = m => `${NAMES[m % 12]}${Math.floor(m / 12) - 1}`;

// ── sources: one per (track, channel) that carries notes ─────────────
const sources = [];
for (const tr of midi.tracks) {
  const byCh = new Map();
  for (const n of tr.notes) { if (!byCh.has(n.ch)) byCh.set(n.ch, []); byCh.get(n.ch).push(n); }
  for (const [ch, notes] of byCh) {
    const pitched = ch !== 9;
    const events = notes.map(n => ({ t: midi.seconds(n.tick) / RATE, dur: Math.max(.03, (midi.seconds(n.end) - midi.seconds(n.tick)) / RATE), midi: pitched ? n.midi + TRANSPOSE : n.midi, vel: n.vel, program: pitched ? midi.programAt(ch, n.tick) : null }))
      .filter(e => e.vel >= MIN_VEL && e.midi >= 0 && e.midi < 128).sort((a, b) => a.t - b.t);
    if (!events.length) continue;
    const progs = [...new Set(events.map(e => e.program))];
    sources.push({ name: tr.name || (pitched ? GM_NAMES[progs[0]] : 'Drums'), track: tr.index, ch, pitched, programs: progs, events,
      lo: Math.min(...events.map(e => e.midi)), hi: Math.max(...events.map(e => e.midi)) });
  }
}
if (!sources.length) throw Error('no notes in this file');
const END = Math.max(...sources.flatMap(s => s.events.map(e => e.t + Math.min(e.dur, MAX_DUR)))) + 2;

// ── voices: attack, decay and the fallback wave by GM family ─────────
const FAMILY = [
  ['piano', .002, .3], ['harp', .002, .25], ['square', .02, .05], ['pluck', .002, .2], ['triangle', .005, .12], ['sawtooth', .05, .15], ['sawtooth', .08, .2], ['sawtooth', .02, .1],
  ['whistle', .03, .1], ['whistle', .04, .1], ['square', .01, .08], ['triangle', .12, .3], ['sine', .05, .2], ['harp', .005, .2], ['harp', .002, .15], ['noise', .01, .1],
];
const voiceOf = program => { const [wave, attack, decayMax] = FAMILY[Math.min(15, Math.floor(program / 8))]; return { wave, attack, decayMax }; };
const gainOf = vel => +(.12 + .5 * Math.pow(vel / 127, 1.4)).toFixed(3);

// ── lanes ─────────────────────────────────────────────────────────────
const lanes = [];
const lane = (name, color, extra = {}) => { const l = { name, color, ...extra, events: [] }; lanes.push(l); return l; };
const push = (l, t, dur, midi, g, wave, attack, decay, extra = {}) => {
  if (!(dur > 0) || !(g > 0)) return;
  const e = { t: r4(t), dur: r4(dur), hz: midi === null ? extra.hz : hz(midi), g, wave, attack: r4(attack), decay: r4(decay) };
  if (midi !== null) e.note = noteName(midi);
  if (Number.isInteger(extra.gm)) e.gm = extra.gm;
  l.events.push(e);
};
const PALETTE = [[255, 110, 110], [255, 180, 70], [120, 220, 130], [95, 170, 255], [200, 130, 255], [255, 240, 200], [255, 150, 200], [150, 230, 230]];
const pitchedSources = sources.filter(s => s.pitched);
let orbitIndex = 0;
for (const s of sources) {
  const k = sources.indexOf(s), color = PALETTE[k % PALETTE.length];
  if (!s.pitched) { drums(s, color); continue; }
  const held = HELD && HELD.test(s.name);
  const pitchedIndex = pitchedSources.indexOf(s), n = pitchedSources.length;
  let laneFor;
  if (held) { const l = lane(`${s.name} (held)`, color, { center: true }); laneFor = () => l; }
  else if (PLACE === 'octave') {
    const ls = NAMES.map((pc, i) => lane(`${s.name} · ${pc}`, noteColor(`${pc}4`) || color, { az: r4(i / 12 * TAU) }));
    laneFor = e => ls[e.midi % 12];
  } else if (PLACE === 'register') {
    const bands = 16, span = Math.max(1, s.hi - s.lo + 1);
    const ls = Array.from({ length: bands }, (_, i) => lane(`${s.name} · ${i + 1}/${bands}`, color, { az: r4(i / bands * TAU) }));
    laneFor = e => ls[Math.min(bands - 1, Math.floor((e.midi - s.lo) / span * bands))];
  } else if (PLACE === 'tracks') {
    const l = lane(s.name, color, ORBIT > 0 ? { azOffset: r4(pitchedIndex / n * TAU), orbitSeconds: ORBIT, orbitDirection: pitchedIndex % 2 ? -1 : 1 } : { az: r4(pitchedIndex / n * TAU) });
    laneFor = () => l;
  } else { // orbit
    const l = lane(s.name, color, { azOffset: r4(pitchedIndex / n * TAU), orbitSeconds: 24 + 12 * orbitIndex, orbitDirection: orbitIndex % 2 ? -1 : 1 }); orbitIndex++;
    laneFor = () => l;
  }
  for (const e of s.events) {
    const v = voiceOf(e.program), dur = Math.min(e.dur, MAX_DUR);
    push(laneFor(e), e.t, dur, e.midi, gainOf(e.vel), v.wave, v.attack, Math.min(v.decayMax, Math.max(.03, dur * .3)), { gm: e.program });
  }
}

// A GM kit on channel 10, in the house recipes: what a laptop can pass.
function drums(s, color) {
  const kick = lane(`${s.name} · kick`, [255, 107, 125], { az: 0 });
  const snare = lane(`${s.name} · snare`, [122, 223, 153], { az: r4(Math.PI) });
  const hats = lane(`${s.name} · hats`, [230, 233, 241], { orbitSeconds: 5 });
  const cymbals = Array.from({ length: RING }, (_, i) => lane(`${s.name} · cymbal ${i + 1}`, [240, 220, 160], { az: r4(i / RING * TAU) }));
  const toms = Array.from({ length: 4 }, (_, i) => lane(`${s.name} · tom ${i + 1}`, color, { az: r4((.15 + i * .2) * TAU) }));
  const other = lane(`${s.name} · other`, color, { orbitSeconds: 9, orbitDirection: -1 });
  for (const e of s.events) {
    const g = gainOf(e.vel), t = e.t, m = e.midi;
    if (m === 35 || m === 36) {
      push(kick, t, .03, null, r4(.3 * g), 'noise', .001, .02, { hz: 2500 });
      push(kick, t, .055, null, r4(.8 * g), 'sine', .001, .04, { hz: 150 });
      push(kick, t + .06, .16, null, r4(.75 * g), 'sine', .001, .11, { hz: 78 });
      push(kick, t + .06, .14, null, r4(.45 * g), 'sine', .001, .1, { hz: 156 });
    } else if (m === 38 || m === 40 || m === 37 || m === 39) {
      push(snare, t, .12, null, r4(.6 * g), 'noise', .001, .1, { hz: 3000 });
      push(snare, t, .08, null, r4(.5 * g), 'sine', .001, .06, { hz: 190 });
    } else if (m === 42 || m === 44) push(hats, t, .04, null, r4(.35 * g), 'noise', .001, .03, { hz: 7000 });
    else if (m === 46) push(hats, t, .25, null, r4(.3 * g), 'noise', .001, .2, { hz: 6000 });
    else if (m === 49 || m === 57 || m === 55 || m === 52) { for (let k = 0; k < RING; k++) push(cymbals[k], t + k * .012, .5, null, r4(.5 * g), 'noise', .002, .4, { hz: 3000 }); }
    else if (m === 51 || m === 59 || m === 53) push(cymbals[Math.floor(RING / 2)], t, .18, null, r4(.4 * g), 'noise', .001, .15, { hz: 5000 });
    else if (m >= 41 && m <= 50) { const i = m <= 43 ? 0 : m <= 45 ? 1 : m <= 47 ? 2 : 3; push(toms[i], t, .18, null, r4(.7 * g), 'sine', .001, .12, { hz: [90, 110, 140, 170][i] }); push(toms[i], t, .03, null, r4(.25 * g), 'noise', .001, .02, { hz: 2000 }); }
    else push(other, t, .06, null, r4(.4 * g), 'noise', .001, .05, { hz: 2000 });
  }
}
for (const l of lanes) l.events.sort((a, b) => a.t - b.t);
const kept = lanes.filter(l => l.events.length);
lanes.length = 0; lanes.push(...kept);

// ── score ─────────────────────────────────────────────────────────────
const slug = opt('name', basename(file).replace(/\.midi?$/i, '')).toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
const title = opt('title', midi.title || basename(file).replace(/\.midi?$/i, ''));
const tempo = midi.tempos.map(t => ({ t: r4(t.sec / RATE), bpm: +(6e7 / t.us * RATE).toFixed(2) }));
const marks = midi.markers.map(m => ({ name: m.name, t: midi.seconds(m.tick) / RATE })).filter((m, i, a) => i === 0 || m.t > a[i - 1].t);
const movements = marks.length >= 2 ? marks.map((m, i) => ({ name: m.name, t0: r4(m.t), t1: r4(i + 1 < marks.length ? marks[i + 1].t : END - 2), level: .65 }))
  : [{ name: title, t0: 0, t1: r4(END - 2), level: .65 }];
const balance = JSON.parse(await readFile(new URL('./notespatial-gm-levels.json', import.meta.url), 'utf8'));
const seatColors = Array.from({ length: SEATS }, (_, i) => PALETTE[i % PALETTE.length]);
const score = {
  name: title, geometry: 'ring', seats: SEATS, ring: RING, ...(CENTER === null ? {} : { center: CENTER }), seatColors,
  dur: r4(END), gain: GAIN, tempo, movements, lanes,
  gmGains: balance.programs.map(p => p.gain), gmBalance: { method: balance.method, target: balance.target, coreHash: balance.coreHash },
  source: { file: basename(file), format: midi.format, division: midi.division, tracks: midi.tracks.length, notes: sources.reduce((n, s) => n + s.events.length, 0) },
  spatializer: { tool: 'midi-to-nsscore', place: PLACE, orbit: ORBIT || undefined, held: HELD?.source, rate: RATE, transpose: TRANSPOSE, maxDur: MAX_DUR },
};
const out = opt('out', null) ? resolve(opt('out')) : new URL(`../scores/${slug}.nsscore`, import.meta.url);
await mkdir(new URL('../scores/', import.meta.url), { recursive: true });
await writeFile(out, JSON.stringify(score) + '\n');

// ── report ────────────────────────────────────────────────────────────
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const all = lanes.flatMap((l, i) => l.events.map(e => ({ ...e, lane: i })));
console.log(`${title}: ${mmss(END)} (${END.toFixed(1)} s), ${sources.length} sources → ${lanes.length} lanes, ${all.length} events, ring of ${RING}${CENTER === null ? '' : ' + center'}, place=${PLACE}`);
console.log('source                         ch  program                  notes   range');
for (const s of sources) console.log(`${s.name.slice(0, 30).padEnd(30)} ${String(s.ch + 1).padStart(2)}  ${(s.pitched ? s.programs.map(p => `${p} ${GM_NAMES[p]}`).join(', ') : 'drum kit').slice(0, 24).padEnd(24)} ${String(s.events.length).padStart(6)}   ${s.pitched ? `${noteName(s.lo)}–${noteName(s.hi)}` : ''}`);
// Per-seat peak: how many events sound at once with ≥ half their power on
// that laptop (the native one-shot pool holds 16; oldest voices are stolen).
const active = Array.from({ length: SEATS }, () => 0), peak = active.slice();
const STEP = .05;
const byT = all.slice().sort((a, b) => a.t - b.t);
let cursor = 0; const live = [];
for (let t = 0; t < END; t += STEP) {
  while (cursor < byT.length && byT[cursor].t <= t) live.push(byT[cursor++]);
  for (let i = live.length - 1; i >= 0; i--) if (live[i].t + live[i].dur <= t) live.splice(i, 1);
  active.fill(0);
  for (const e of live) { const pos = voicePosition(score, e.lane, t); for (let s = 0; s < SEATS; s++) if (sourceGain(score, pos, s, SEATS) ** 2 >= .5) active[s]++; }
  for (let s = 0; s < SEATS; s++) if (active[s] > peak[s]) peak[s] = active[s];
}
console.log(`peak voices per seat: ${peak.map((p, i) => `${i + 1}:${p}`).join('  ')}  (native pool 16, oldest stolen)`);
console.log(`→ ${typeof out === 'string' ? out : out.pathname}`);
