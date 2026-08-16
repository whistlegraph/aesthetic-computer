// score.mjs — the one source of truth for pop/blackboard.
//
// A /pop track sung from Tim Ingold's "Thinking through Making" lecture —
// the blackboard-vs-PowerPoint passage (07:28–08:28). The passage's own
// argument dictates the form: projection (the verses, arguing against) vs
// trace (the bridge, one patient performance), resolved by the hook —
// "That's why I like blackboards, and don't like PowerPoint."
//
// 72 BPM · E minor · 48 bars ≈ 2:44. Every module (bed renderer, sing
// driver, texture) reads THIS file; nothing hardcodes a time twice.

export const BPM = 72;
export const SPB = 60 / BPM;          // 0.8333 s per beat
export const BAR = SPB * 4;           // 3.3333 s per bar
export const BARS = 48;
export const TAIL_S = 4;              // ring-out after the last bar
export const DURATION_S = BARS * BAR + TAIL_S;   // 164 s
export const SR = 48_000;

export const bt = (bar, beat = 0) => bar * BAR + beat * SPB;   // absolute seconds

// ── harmony ────────────────────────────────────────────────────────────────
// E minor. Verse walks i–VI–iv–VII and returns home dark; the hook is the
// PROGRESSIONS_CHILL row [0,5,2,6] = i VI III VII (Em C G D) — the same
// pool pop/cult's v2 lifted from recap/bin/trance.mjs (see its README).
// Triads as midi pitch-class stacks; the bed voices them where it likes.
const CH = {
  Em: { root: 40, tones: [52, 55, 59] },   // E2 · E3 G3 B3
  C:  { root: 36, tones: [52, 55, 60] },   // C2 · E3 G3 C4
  Am: { root: 45, tones: [52, 57, 60] },   // A2 · E3 A3 C4
  D:  { root: 38, tones: [54, 57, 62] },   // D2 · F#3 A3 D4
  G:  { root: 43, tones: [55, 59, 62] },   // G2 · G3 B3 D4
};

// chord per bar, 0..47
const P = (...names) => names.map((n) => ({ name: n, ...CH[n] }));
export const CHORDS = P(
  "Em", "Em", "Em", "Em",                       // 0–3   intro
  "Em", "Em", "C", "C", "Am", "Am", "D", "D",   // 4–11  verse (projection)
  "Em", "Em",                                   // 12–13 "…arguing against"
  "C", "C", "Am", "Am", "D", "D",               // 14–19 pre (the reason)
  "Em", "C", "G", "D",                          // 20–23 HOOK 1
  "Am", "Am", "Em", "Em", "C", "C", "D", "D",   // 24–31 bridge (the trace)
  "Em", "C", "G", "D",                          // 32–35 HOOK 2
  "C", "Am", "Em", "D",                         // 36–39 outcome
  "Em", "C", "G", "D",                          // 40–43 HOOK 3
  "Em", "C", "Em", "Em",                        // 44–47 outro
);

export const SECTIONS = [
  { name: "intro",   bar0: 0,  bar1: 4 },
  { name: "verse",   bar0: 4,  bar1: 14 },
  { name: "pre",     bar0: 14, bar1: 20 },
  { name: "hook1",   bar0: 20, bar1: 24 },
  { name: "bridge",  bar0: 24, bar1: 32 },
  { name: "hook2",   bar0: 32, bar1: 36 },
  { name: "outcome", bar0: 36, bar1: 40 },
  { name: "hook3",   bar0: 40, bar1: 44 },
  { name: "outro",   bar0: 44, bar1: 48 },
];

// ── the sung lines ─────────────────────────────────────────────────────────
// Explicit mode (menuband-chords pattern): every syllable hand-placed as
// [t, dur, midi]. The engine's per-line octave fit normalizes the absolute
// octave toward jeffrey's spoken take, so what these midis really encode is
// CONTOUR + pitch class; `register` (semitones above the fit, with a
// fallback ladder if the whisper gate fails up there) sets how high a line
// actually sits. Verses and bridge stay at 0 — chant-dark, close to speech.
// The hook asks +12; "I'm arguing for." and the outcome ask +7.
//
// "PowerPoint" is TTS'd as one word but scored as "power"+"point" — both
// are clean GenAm dictionary words, and the driver's presplit handles the
// welded whisper token (that is exactly what presplitHeard is for).
const N = (bar, beat, durBeats, midi) =>
  ({ t: +bt(bar, beat).toFixed(4), dur: +(durBeats * SPB).toFixed(4), midi });

const hook = (h, li) => ({
  id: `hook-${li}`,
  tts: "That's why I like blackboards, and don't like PowerPoint.",
  register: 12, fallbacks: [7, 0],
  words: [
    ["that's", [N(h, 0, 0.5, 59)]],
    ["why",    [N(h, 0.5, 0.75, 64)]],
    ["i",      [N(h, 1.25, 0.5, 62)]],
    ["like",   [N(h, 1.75, 0.75, 59)]],
    ["black",  [N(h, 2.5, 1.5, 55)]],
    ["boards", [N(h + 1, 0, 2.0, 52)]],      // the falling chalk stroke G→E
    ["and",    [N(h + 2, 0, 0.5, 57)]],
    ["don't",  [N(h + 2, 0.5, 0.75, 59)]],
    ["like",   [N(h + 2, 1.25, 0.75, 57)]],
    ["power",  [N(h + 2, 2, 0.75, 55), N(h + 2, 2.75, 0.75, 54)]],
    ["point",  [N(h + 3, 0, 2.5, 52)]],      // 9th over D — suspended, patient
  ],
});

export const LINES = [
  // ── verse — projection, argued against ──────────────────────────────────
  { id: "v1", tts: "I don't believe in PowerPoint.", register: 0,
    words: [
      ["i",       [N(4, 0, 0.75, 52)]],
      ["don't",   [N(4, 1, 0.75, 55)]],
      ["believe", [N(4, 2, 0.5, 57), N(4, 2.5, 1.25, 59)]],
      ["in",      [N(5, 0, 0.5, 57)]],
      ["power",   [N(5, 1, 0.5, 55), N(5, 1.5, 0.5, 54)]],
      ["point",   [N(5, 2, 1.5, 52)]],
    ] },
  { id: "v2", tts: "When we use PowerPoint, we project images on a screen.", register: 0,
    words: [
      ["when",    [N(6, 0, 0.5, 55)]],
      ["we",      [N(6, 0.5, 0.5, 57)]],
      ["use",     [N(6, 1, 1.25, 60)]],
      ["power",   [N(6, 2.5, 0.5, 59), N(6, 3, 0.5, 57)]],
      ["point",   [N(6, 3.5, 1.25, 55)]],
      ["we",      [N(7, 1.5, 0.5, 55)]],
      ["project", [N(7, 2, 0.5, 57), N(7, 2.5, 1.0, 59)]],
      ["images",  [N(8, 0, 0.5, 57), N(8, 0.5, 0.5, 55), N(8, 1, 0.5, 52)]],
      ["on",      [N(8, 2, 0.5, 55)]],
      ["a",       [N(8, 2.5, 0.5, 57)]],
      ["screen",  [N(8, 3, 1.5, 59)]],
    ] },
  { id: "v3", tts: "PowerPoint is the epitome of the logic of projection.", register: 0,
    words: [
      ["power",      [N(9, 0, 0.5, 57), N(9, 0.5, 0.5, 55)]],
      ["point",      [N(9, 1, 1.0, 52)]],
      ["is",         [N(9, 2.5, 0.5, 55)]],
      ["the",        [N(9, 3, 0.5, 57)]],
      ["epitome",    [N(9, 3.5, 0.5, 59), N(10, 0, 1.0, 62), N(10, 1, 0.5, 59), N(10, 1.5, 0.5, 57)]],
      ["of",         [N(10, 2, 0.5, 55)]],
      ["the",        [N(10, 2.5, 0.5, 54)]],
      ["logic",      [N(10, 3, 0.5, 57), N(10, 3.5, 0.5, 54)]],
      ["of",         [N(11, 0, 0.5, 52)]],
      ["projection", [N(11, 0.5, 0.5, 54), N(11, 1, 1.0, 57), N(11, 2, 1.0, 54)]],
    ] },
  { id: "v4", tts: "I'm arguing against.", register: 0,
    words: [
      ["i'm",     [N(12, 0, 0.75, 55)]],
      ["arguing", [N(12, 1, 0.75, 52), N(12, 2, 0.5, 55), N(12, 2.5, 0.5, 54)]],
      ["against", [N(12, 3, 0.5, 52), N(12, 3.5, 2.0, 47)]],   // falls to B2, held
    ] },

  // ── pre — the reason, argued for ────────────────────────────────────────
  { id: "p1", tts: "And the reason why I like blackboards.", register: 0,
    words: [
      ["and",    [N(14, 0, 0.5, 55)]],
      ["the",    [N(14, 0.5, 0.5, 55)]],
      ["reason", [N(14, 1, 0.5, 60), N(14, 1.5, 0.5, 59)]],
      ["why",    [N(14, 2, 0.75, 57)]],
      ["i",      [N(14, 3, 0.5, 55)]],
      ["like",   [N(14, 3.5, 0.75, 57)]],
      ["black",  [N(15, 0.5, 1.0, 60)]],
      ["boards", [N(15, 1.5, 1.5, 55)]],
    ] },
  { id: "p2", tts: "The blackboard is the epitome of the process of creativity.", register: 0,
    words: [
      ["the",        [N(16, 0, 0.5, 52)]],
      ["black",      [N(16, 0.5, 0.75, 57)]],
      ["board",      [N(16, 1.25, 0.75, 55)]],
      ["is",         [N(16, 2, 0.5, 52)]],
      ["the",        [N(16, 2.5, 0.5, 55)]],
      ["epitome",    [N(16, 3, 0.5, 57), N(16, 3.5, 1.0, 60), N(17, 0.5, 0.5, 59), N(17, 1, 0.5, 57)]],
      ["of",         [N(17, 1.5, 0.5, 55)]],
      ["the",        [N(17, 2, 0.5, 55)]],
      ["process",    [N(17, 2.5, 0.5, 57), N(17, 3, 0.5, 55)]],
      ["of",         [N(17, 3.5, 0.5, 52)]],
      ["creativity", [N(18, 0, 0.5, 54), N(18, 0.5, 0.75, 57), N(18, 1.25, 0.5, 59),
                      N(18, 1.75, 0.5, 57), N(18, 2.25, 1.25, 54)]],
    ] },
  { id: "p3", tts: "I'm arguing for.", register: 7, fallbacks: [0],
    words: [
      ["i'm",     [N(19, 0, 0.75, 54)]],
      ["arguing", [N(19, 1, 0.75, 57), N(19, 2, 0.5, 59), N(19, 2.5, 0.5, 62)]],
      ["for",     [N(19, 3, 1.8, 64)]],   // rises a ninth — the mirror of "against"
    ] },

  // ── hook 1 ──────────────────────────────────────────────────────────────
  hook(20, 1),

  // ── bridge — the trace: one performance ─────────────────────────────────
  { id: "b1", tts: "When you stand at the blackboard, and you scrape a line.", register: 0,
    words: [
      ["when",   [N(24, 0, 0.5, 52)]],
      ["you",    [N(24, 0.5, 0.5, 55)]],
      ["stand",  [N(24, 1, 1.0, 57)]],
      ["at",     [N(24, 2.5, 0.5, 55)]],
      ["the",    [N(24, 3, 0.5, 57)]],
      ["black",  [N(24, 3.5, 1.0, 60)]],
      ["board",  [N(25, 0.5, 1.25, 57)]],
      ["and",    [N(25, 2.5, 0.5, 55)]],
      ["you",    [N(25, 3, 0.5, 57)]],
      ["scrape", [N(25, 3.5, 1.5, 59)]],   // the /skr/ held across the barline
      ["a",      [N(26, 1.5, 0.5, 57)]],
      ["line",   [N(26, 2, 1.8, 55)]],
    ] },
  { id: "b2", tts: "Your movement, your awareness, the trace of the materials.", register: 0,
    words: [
      ["your",      [N(27, 0, 0.5, 52)]],
      ["movement",  [N(27, 0.5, 0.75, 55), N(27, 1.25, 0.75, 52)]],
      ["your",      [N(27, 2.5, 0.5, 52)]],
      ["awareness", [N(27, 3, 0.5, 55), N(27, 3.5, 1.0, 59), N(28, 0.5, 0.75, 55)]],
      ["the",       [N(28, 2, 0.5, 55)]],
      ["trace",     [N(28, 2.5, 1.0, 60)]],
      ["of",        [N(28, 3.5, 0.5, 59)]],
      ["the",       [N(29, 0, 0.5, 57)]],
      ["materials", [N(29, 0.5, 0.5, 55), N(29, 1, 0.75, 57), N(29, 1.75, 0.5, 55),
                     N(29, 2.25, 1.25, 52)]],
    ] },
  { id: "b3", tts: "All bound up in that one performance.", register: 0,
    words: [
      ["all",         [N(30, 0, 0.75, 54)]],
      ["bound",       [N(30, 1, 1.0, 57)]],
      ["up",          [N(30, 2, 0.5, 54)]],
      ["in",          [N(30, 2.5, 0.5, 52)]],
      ["that",        [N(30, 3, 0.5, 54)]],
      ["one",         [N(30, 3.5, 1.0, 57)]],
      ["performance", [N(31, 1, 0.5, 54), N(31, 1.5, 1.0, 59), N(31, 2.5, 1.5, 62)]],
    ] },

  // ── hook 2 ──────────────────────────────────────────────────────────────
  hook(32, 2),

  // ── outcome ─────────────────────────────────────────────────────────────
  { id: "o1", tts: "And what you see is the outcome of that performance.", register: 7, fallbacks: [0],
    words: [
      ["and",         [N(36, 0, 0.5, 55)]],
      ["what",        [N(36, 0.5, 0.5, 57)]],
      ["you",         [N(36, 1, 0.5, 59)]],
      ["see",         [N(36, 1.5, 1.25, 60)]],
      ["is",          [N(36, 3, 0.5, 59)]],
      ["the",         [N(36, 3.5, 0.5, 57)]],
      ["outcome",     [N(37, 0, 0.75, 60), N(37, 0.75, 0.75, 57)]],
      ["of",          [N(37, 1.5, 0.5, 55)]],
      ["that",        [N(37, 2, 0.5, 54)]],
      ["performance", [N(37, 2.5, 0.5, 52), N(37, 3, 1.0, 55), N(38, 0, 2.0, 52)]],
    ] },

  // ── hook 3 ──────────────────────────────────────────────────────────────
  hook(40, 3),
];

// The hook melody for the bell double (bed side): flattened hook slots.
export const HOOK_MELODY = hook(0, 0).words.flatMap(([, slots]) => slots);
