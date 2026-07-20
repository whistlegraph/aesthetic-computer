// GM (General MIDI) sample-based player.
//
// Loads pitched melodic patches and the GM standard drum kit from the
// AC asset CDN and plays them through the existing browser AudioContext
// (`window.audioContext`, set up by bios.mjs). Each patch is sparsely
// rendered (every 3rd semitone) so this player pitch-shifts the nearest
// rendered sample via Web Audio's `playbackRate` to fill in missing
// notes, applies a snappy ADSR-ish gain envelope, and exposes per-note
// stop()/release() handles.
//
// Asset layout (produced by the lith bake script — see lith/):
//   https://assets.aesthetic.computer/gm/manifest.json
//   https://assets.aesthetic.computer/gm/<NNN>/<note>.mp3       (NNN = 000..127)
//   https://assets.aesthetic.computer/gm/drum-000/<note>.mp3
//
// Note files use scientific-pitch names with `s` for sharps:
//   C4.mp3, Cs4.mp3, D4.mp3, Ds4.mp3, ... A4.mp3, As4.mp3, B4.mp3
//
// This module has NO build step — it is a plain ES module loaded by the
// runtime. It does not modify disk.mjs, midi.mjs, or any disk file; the
// program-change wiring will be hooked up elsewhere.

// Local dev (`npm run site`) serves system/public/assets/ at /assets/, so
// the bake output dropped into system/public/assets/gm/ is reachable
// without uploading to the CDN. On prod we hit the Spaces-backed CDN.
function resolveAssetBase() {
  if (typeof window === "undefined") return "https://assets.aesthetic.computer/gm";
  const host = window.location?.hostname || "";
  const isLocal =
    host === "localhost" ||
    host === "127.0.0.1" ||
    host === "" ||
    host.endsWith(".local") ||
    host.endsWith(".github.dev") ||
    host.endsWith(".gitpod.io");
  return isLocal ? "/assets/gm" : "https://assets.aesthetic.computer/gm";
}
let ASSET_BASE = resolveAssetBase();

// Standalone consumers (outside the full AC dev server) can point the player
// at the shared CDN explicitly. Calling this before loadPatch/loadManifest is
// enough; the ordinary Notepat piece keeps the existing auto-resolution.
export function setGMAssetBase(base) {
  if (!base) return ASSET_BASE;
  ASSET_BASE = String(base).replace(/\/$/, "");
  _manifest = null;
  _manifestPromise = null;
  _patchCache.clear();
  _bufferCache.clear();
  return ASSET_BASE;
}

// ─── AudioContext access ──────────────────────────────────────────────
// AC's bios.mjs creates and stores a single AudioContext at
// `window.audioContext`. All other lib helpers either receive a `sound`
// API or read the global. We accept an explicit context for testability
// and fall back to the window global.
function resolveAudioContext(audioContext) {
  if (audioContext) return audioContext;
  const w = typeof window !== "undefined" ? /** @type {any} */ (window) : null;
  if (w && w.audioContext) return w.audioContext;
  return null;
}

// ─── Note name <-> MIDI helpers ───────────────────────────────────────
// Sample filenames use `s` (e.g. "Cs4") instead of "#" because URL
// safety. Octave numbering is scientific-pitch: C4 = MIDI 60.
const NOTE_TO_PC = {
  C: 0, Cs: 1, D: 2, Ds: 3, E: 4, F: 5,
  Fs: 6, G: 7, Gs: 8, A: 9, As: 10, B: 11,
};
const PC_TO_NOTE = ["C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B"];

export function midiToNoteName(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const octave = Math.floor(midi / 12) - 1;
  return `${PC_TO_NOTE[pc]}${octave}`;
}

export function noteNameToMidi(name) {
  const m = /^([A-G])(s?)(-?\d+)$/.exec(name);
  if (!m) return null;
  const letter = m[1] + (m[2] || "");
  const pc = NOTE_TO_PC[letter];
  if (pc === undefined) return null;
  const octave = parseInt(m[3], 10);
  return (octave + 1) * 12 + pc;
}

// ─── Manifest ─────────────────────────────────────────────────────────
// The bake script writes a manifest.json describing which programs are
// available, which notes were rendered per program, sample rate, etc.
// We tolerate a missing/incomplete manifest by falling back to a
// default note grid (every 3rd semitone over MIDI 24..96).
let _manifest = null;
let _manifestPromise = null;

const DEFAULT_NOTE_STEP = 3;
const DEFAULT_LOW = 24;   // C1
const DEFAULT_HIGH = 96;  // C7

function defaultNoteList() {
  const list = [];
  for (let m = DEFAULT_LOW; m <= DEFAULT_HIGH; m += DEFAULT_NOTE_STEP) {
    list.push(m);
  }
  return list;
}

export async function loadManifest(audioContext) {
  // audioContext is accepted for API symmetry with loadPatch / loadDrumKit
  // (they need it to decode); manifest loading itself is pure JSON.
  void audioContext;
  if (_manifest) return _manifest;
  if (_manifestPromise) return _manifestPromise;

  _manifestPromise = (async () => {
    try {
      const res = await fetch(`${ASSET_BASE}/manifest.json`, { cache: "force-cache" });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const json = await res.json();
      _manifest = normalizeManifest(json);
    } catch (err) {
      console.warn("🎼 GM: manifest unavailable, using defaults:", err.message);
      _manifest = {
        patches: {},
        drumKits: { 0: { notes: defaultNoteList() } },
        noteStep: DEFAULT_NOTE_STEP,
        defaultNotes: defaultNoteList(),
        _fallback: true,
      };
    }
    return _manifest;
  })();

  return _manifestPromise;
}

function normalizeManifest(raw) {
  const out = {
    patches: {},
    drumKits: {},
    noteStep: raw.noteStep || raw.step || DEFAULT_NOTE_STEP,
    defaultNotes: null,
    _raw: raw,
  };
  // Patches may be an array, an object keyed by program number, or a
  // top-level `patches` field. Normalize to { [program]: { notes:[...] } }.
  const patchSrc = raw.patches || raw.programs || raw;
  if (Array.isArray(patchSrc)) {
    for (const entry of patchSrc) {
      const program = entry?.program ?? entry?.id;
      if (program != null) {
        out.patches[program] = { notes: entry.notes || null, name: entry.name };
      }
    }
  } else if (patchSrc && typeof patchSrc === "object") {
    for (const [k, v] of Object.entries(patchSrc)) {
      const pn = parseInt(k, 10);
      if (!Number.isFinite(pn) || pn < 0 || pn > 127) continue;
      if (v && typeof v === "object") {
        out.patches[pn] = { notes: v.notes || null, name: v.name };
      }
    }
  }
  const drumSrc = raw.drumKits || raw.drums || {};
  for (const [k, v] of Object.entries(drumSrc)) {
    const id = parseInt(k, 10);
    if (!Number.isFinite(id)) continue;
    out.drumKits[id] = { notes: v?.notes || null, name: v?.name };
  }
  if (!out.drumKits[0]) out.drumKits[0] = { notes: null };
  out.defaultNotes = defaultNoteList();
  return out;
}

// ─── Buffer fetch + decode (cached per URL) ───────────────────────────
const _bufferCache = new Map(); // url -> Promise<AudioBuffer | null>

function fetchAndDecode(url, ctx) {
  if (_bufferCache.has(url)) return _bufferCache.get(url);
  const p = (async () => {
    try {
      const res = await fetch(url, { cache: "force-cache" });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const arrayBuf = await res.arrayBuffer();
      // decodeAudioData has both promise and callback forms. Use the
      // promise form; old Safari is wrapped by the AC bios so it should
      // be fine here.
      const audioBuf = await ctx.decodeAudioData(arrayBuf);
      return audioBuf;
    } catch (err) {
      console.warn(`🎼 GM: failed to load ${url}:`, err.message);
      return null;
    }
  })();
  _bufferCache.set(url, p);
  return p;
}

// ─── Patch / kit handles ──────────────────────────────────────────────
// A patch handle keeps its set of rendered MIDI notes and lazily loads
// any individual sample on demand. `play()` finds the nearest rendered
// note, pitch-shifts it, and schedules with an ADSR-ish envelope.

const _patchCache = new Map(); // programNumber -> Promise<patchHandle>
const _drumCache = new Map();  // kitId -> Promise<kitHandle>

function programDir(programNumber) {
  return `${ASSET_BASE}/${String(programNumber).padStart(3, "0")}`;
}

function drumDir(kitId) {
  return `${ASSET_BASE}/drum-${String(kitId).padStart(3, "0")}`;
}

// Build the URL for a single rendered note (e.g. midi 60 -> ".../C4.mp3").
function noteUrl(dir, midi) {
  return `${dir}/${midiToNoteName(midi)}.mp3`;
}

// Drum samples are filed by raw MIDI number (e.g. 35 = Acoustic Bass Drum)
// because drum kits aren't pitched — scientific-pitch names ("B1") would be
// semantically wrong. Matches the bake script's output convention.
function drumNoteUrl(dir, midi) {
  return `${dir}/${midi}.mp3`;
}

// Pick the closest rendered MIDI note to a target.
function nearestRenderedNote(noteList, target) {
  if (!noteList || noteList.length === 0) return null;
  let best = noteList[0];
  let bestDist = Math.abs(best - target);
  for (let i = 1; i < noteList.length; i++) {
    const d = Math.abs(noteList[i] - target);
    if (d < bestDist) {
      best = noteList[i];
      bestDist = d;
    }
  }
  return best;
}

// No-op note handle returned when something fails — keeps callers safe.
function noopNoteHandle(reason) {
  return {
    stop() {},
    release() {},
    _failed: true,
    _reason: reason,
  };
}

// Schedule a single sample with envelope. Returns a noteHandle.
function scheduleNote(ctx, audioBuffer, opts) {
  const {
    playbackRate = 1,
    velocity = 100,
    when = 0,
    duration,         // optional; if omitted note sustains until release()
    attack = 0.005,   // 5ms snappy
    decay = 0.04,
    sustain = 0.85,   // fraction of peak
    release = 0.15,   // 150ms
    destination,
  } = opts;

  const startTime = when || ctx.currentTime;
  const peak = Math.max(0, Math.min(1, velocity / 127));

  let source, gain;
  try {
    source = ctx.createBufferSource();
    source.buffer = audioBuffer;
    source.playbackRate.value = playbackRate;

    gain = ctx.createGain();
    gain.gain.setValueAtTime(0, startTime);
    gain.gain.linearRampToValueAtTime(peak, startTime + attack);
    gain.gain.linearRampToValueAtTime(
      peak * sustain,
      startTime + attack + decay,
    );

    source.connect(gain);
    gain.connect(destination || ctx.destination);

    source.start(startTime);
  } catch (err) {
    console.warn("🎼 GM: scheduleNote failed:", err);
    return noopNoteHandle("schedule-failed");
  }

  let stopped = false;
  let scheduledStop = null;

  const releaseAt = (t) => {
    if (stopped) return;
    stopped = true;
    const time = Math.max(t, ctx.currentTime);
    try {
      gain.gain.cancelScheduledValues(time);
      // Hold current value, then ramp to 0 over `release` seconds.
      const current = gain.gain.value;
      gain.gain.setValueAtTime(current, time);
      gain.gain.linearRampToValueAtTime(0.0001, time + release);
      // Schedule actual stop slightly after the release fade.
      scheduledStop = time + release + 0.02;
      source.stop(scheduledStop);
    } catch (err) {
      // Already stopped or invalid state — ignore.
    }
  };

  // If a fixed duration was supplied, schedule the release at start+dur.
  if (typeof duration === "number" && duration > 0) {
    releaseAt(startTime + duration);
  }

  return {
    stop(when = 0) {
      releaseAt(when || ctx.currentTime);
    },
    release() {
      releaseAt(ctx.currentTime);
    },
    get _node() { return source; },
    get _gain() { return gain; },
  };
}

// ─── Patch loader ─────────────────────────────────────────────────────
export async function loadPatch(programNumber, audioContext) {
  const program = programNumber | 0;
  if (program < 0 || program > 127) {
    console.warn("🎼 GM: program out of range:", programNumber);
    return null;
  }

  if (_patchCache.has(program)) return _patchCache.get(program);

  const p = (async () => {
    const ctx = resolveAudioContext(audioContext);
    if (!ctx) {
      console.warn("🎼 GM: no AudioContext available");
      return null;
    }

    const manifest = await loadManifest(ctx);
    const patchEntry = manifest.patches[program];
    const noteList =
      (patchEntry && patchEntry.notes) || manifest.defaultNotes || defaultNoteList();
    const dir = programDir(program);

    // Per-note buffer cache scoped to this patch — the inner URL cache
    // dedupes across patches if any happen to share notes.
    const samples = new Map(); // midi -> Promise<AudioBuffer | null>

    function getSample(midi) {
      if (samples.has(midi)) return samples.get(midi);
      const promise = fetchAndDecode(noteUrl(dir, midi), ctx);
      samples.set(midi, promise);
      return promise;
    }

    return {
      program,
      name: patchEntry?.name || `program-${program}`,
      notes: noteList,
      // Warm the cache for a single rendered note (or all of them).
      async prefetch(midi) {
        if (typeof midi === "number") return getSample(midi);
        return Promise.all(noteList.map((m) => getSample(m)));
      },
      play(midiNote, opts = {}) {
        const target = midiNote | 0;
        const sampleNote = nearestRenderedNote(noteList, target);
        if (sampleNote == null) return noopNoteHandle("no-samples");
        const playbackRate = Math.pow(2, (target - sampleNote) / 12);

        let live = noopNoteHandle("pending");
        let externalReleased = false;

        getSample(sampleNote).then((buf) => {
          if (!buf) {
            live = noopNoteHandle("decode-failed");
            return;
          }
          if (externalReleased) {
            // Caller already released before the buffer arrived; skip.
            return;
          }
          live = scheduleNote(ctx, buf, {
            ...opts,
            playbackRate,
          });
        });

        return {
          stop(when = 0) {
            externalReleased = true;
            live?.stop?.(when);
          },
          release() {
            externalReleased = true;
            live?.release?.();
          },
        };
      },
    };
  })();

  _patchCache.set(program, p);
  return p;
}

// Warm a patch's samples without playing anything.
export async function prefetchPatch(programNumber, audioContext) {
  const patch = await loadPatch(programNumber, audioContext);
  if (!patch) return null;
  await patch.prefetch();
  return patch;
}

// ─── Drum kit loader ──────────────────────────────────────────────────
// GM percussion uses MIDI channel 10 with one-shot samples per MIDI
// note number (35..81 in the standard kit). Drum hits don't pitch-shift
// — we play whichever exact note was requested if rendered, otherwise
// fall back to the nearest rendered note WITHOUT changing playbackRate
// (changing rate would distort the timbre). A future option flag could
// re-enable pitched drums.
export async function loadDrumKit(kitId = 0, audioContext) {
  const id = kitId | 0;
  if (_drumCache.has(id)) return _drumCache.get(id);

  const p = (async () => {
    const ctx = resolveAudioContext(audioContext);
    if (!ctx) return null;
    const manifest = await loadManifest(ctx);
    const kitEntry = manifest.drumKits[id] || manifest.drumKits[0];
    const noteList = (kitEntry && kitEntry.notes) || manifest.defaultNotes || defaultNoteList();
    const dir = drumDir(id);
    const samples = new Map();

    function getSample(midi) {
      if (samples.has(midi)) return samples.get(midi);
      const promise = fetchAndDecode(drumNoteUrl(dir, midi), ctx);
      samples.set(midi, promise);
      return promise;
    }

    return {
      kitId: id,
      name: kitEntry?.name || `drum-${id}`,
      notes: noteList,
      async prefetch(midi) {
        if (typeof midi === "number") return getSample(midi);
        return Promise.all(noteList.map((m) => getSample(m)));
      },
      play(midiNote, opts = {}) {
        const target = midiNote | 0;
        const sampleNote = nearestRenderedNote(noteList, target);
        if (sampleNote == null) return noopNoteHandle("no-samples");

        let live = noopNoteHandle("pending");
        let externalReleased = false;

        getSample(sampleNote).then((buf) => {
          if (!buf) { live = noopNoteHandle("decode-failed"); return; }
          if (externalReleased) return;
          // Drums: no pitch shift, very short release, let the sample's
          // own decay carry the tail.
          live = scheduleNote(ctx, buf, {
            ...opts,
            playbackRate: 1,
            attack: 0.001,
            decay: 0.01,
            sustain: 1.0,
            release: 0.03,
          });
        });

        return {
          stop(when = 0) { externalReleased = true; live?.stop?.(when); },
          release() { externalReleased = true; live?.release?.(); },
        };
      },
    };
  })();

  _drumCache.set(id, p);
  return p;
}

// ─── Cache controls (mostly for tests / hot reload) ───────────────────
export function _resetGMCaches() {
  _bufferCache.clear();
  _patchCache.clear();
  _drumCache.clear();
  _manifest = null;
  _manifestPromise = null;
}

// ──────────────────────────────────────────────────────────────────────
// Self-test snippet — paste into a piece's `boot` to sanity-check.
// (Requires the bake script to have populated assets.aesthetic.computer.)
//
// import { loadPatch, loadDrumKit, prefetchPatch } from "/aesthetic.computer/lib/gm.mjs";
//
// async function boot({ sound }) {
//   // 1. Acoustic Grand Piano (GM program 0):
//   const piano = await loadPatch(0);
//   const note = piano.play(60, { velocity: 110 });    // middle C
//   setTimeout(() => note.release(), 800);
//
//   // 2. Pitch-shifted note that wasn't directly rendered:
//   piano.play(61, { velocity: 90, duration: 0.5 });   // C# via nearest sample
//
//   // 3. Standard drum kit — kick (MIDI 36) and snare (MIDI 38):
//   const kit = await loadDrumKit(0);
//   kit.play(36); setTimeout(() => kit.play(38), 250);
//
//   // 4. Warm a patch ahead of time:
//   prefetchPatch(24); // nylon guitar
// }
