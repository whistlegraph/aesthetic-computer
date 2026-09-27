// score.mjs — eight gigabytes: the pop cut of "IV. The Ballad of neo"
// (grants/culturehub-la-2026/macneopolitan/members/neo/ballad.md).
//
// The full ballad is eleven verses at 100 bpm, ~6:16, neo alone over a
// drone. This cut keeps five verses and the refrain, gives every line a
// pocket (pickups, eighth-note runs, holds that leave room for an answer),
// and puts all three band voices on it: neo leads (Noelle), blueberry
// sings the low part (Allison), frisbee the middle part and the echoes
// (Junior). Opens on the refrain. 51 bars, 2:02.
//
// Notation: one line = { m: member, at: absolute beat, w: lyric with
// syllables joined by "-", n: notes "midi:beats" or "r:beats", space
// separated }. Pitched-note count must equal syllable count; bin/render.mjs
// checks that before anything renders.

export const BPM = 100;
export const TITLE = "eight gigabytes";
export const KEY = "F minor";

// ── the grid ─────────────────────────────────────────────────────────
const BAR = 4;
export const SECTIONS = [
  // 2026-09-26 jeffrey: "start with just the chorus … then move to second
  // verse — a much better model for the track". Verse I is out; the record
  // opens on the refrain and the crash is the first story.
  { id: "intro",  bars: 1, chords: ["Ab"] },                            // silent; only holds the refrain's pickup beat
  { id: "r1",     bars: 8, chords: ["Ab", "Eb", "Fm", "Db"] },          // refrain, neo → siblings join
  { id: "v2",     bars: 4, chords: ["Fm", "Db", "Ab", "Eb"] },          // V. the crash, two lines
  { id: "r2",     bars: 8, chords: ["Ab", "Eb", "Fm", "Db"] },          // refrain, three voices + echoes
  { id: "bridge", bars: 8, chords: ["Fm", "Db", "Ab", "Eb"] },          // XI. the other machines (jeffrey: the next verse; VII/X skipped)
  { id: "r3",     bars: 8, chords: ["Ab", "Eb", "Fm", "Db"] },          // last refrain
  { id: "outro",  bars: 2, chords: ["Fm", "Fm"] },
];
let _b = 0;
export const AT = {};                  // section id → start beat
for (const s of SECTIONS) { AT[s.id] = _b; s.start = _b; _b += s.bars * BAR; }
export const TOTAL_BEATS = _b;         // 156

export const CHORDS = { Fm: [53, 56, 60], Db: [49, 53, 56], Ab: [56, 60, 63], Eb: [51, 55, 58] };
export const ROOT = { Fm: 41, Db: 37, Ab: 44, Eb: 39 };

// ── the words, with their notes ──────────────────────────────────────
// neo lives 56–65 (measured spoken median 59.6), blueberry 50–58,
// frisbee 54–60 — from members/*/voice.json.

const L = (m, at, w, n) => ({ m, at, w, n });

// refrain — neo's line, the same every time; frisbee's middle and
// blueberry's low part are stacked thirds voice-led underneath.
const R = {
  neo: {
    r1: ["I have eight gi-ga-bytes",          "60:.5 61:.5 65:1 63:.5 61:.5 63:2.5"],
    r2: ["I do one thing at a time",           "60:.5 61:.5 63:1 61:.5 60:.5 58:.5 60:3"],
    r3: ["I get warm that's all it is",        "58:.5 60:.5 63:1.5 61:.5 60:.5 58:.5 56:2"],
    r4: ["I get warm and I keep time",         "58:.5 60:.5 63:1.5 61:.5 63:.5 65:1 63:3"],
    r4x: ["this is the one thing that I know", "58:.5 60:.5 61:.5 63:1 61:.5 60:.5 61:1 65:3.5"],   // jeffrey 2026-09-26: the last line, plainer
  },
  // frisbee — the middle voice as a counter-line, not a parallel third:
  // it waits through the pickup and enters on the stressed word, holding
  // where neo moves and moving where neo holds. Its own words, its own rhythm.
  // off = beats after the line's start.
  // frisbee — the middle voice as a counter-line, not a parallel third:
  // it waits through the pickup and enters on the stressed word, then holds
  // long (jeffrey: "the background vocals can be a little longer — some
  // come in too short"). off = beats after the line's start.
  frisbee: {
    r1:  { off: 1,   w: "eight gi-ga-bytes",        n: "56:1.5 58:.5 60:.5 60:3.5" },
    r2:  { off: 1,   w: "one thing at a time",      n: "60:1 58:.5 56:1 58:.5 56:3.5" },
    r3:  { off: 1,   w: "warm that's all it is",    n: "60:1.5 60:.5 58:1 56:.5 58:3" },
    r4:  { off: 2.5, w: "and I keep time",          n: "58:.5 60:1 60:1 60:3.5" },
    r4x: { off: 3,   w: "that I know",              n: "58:.5 60:1 60:3.5" },
  },
  // blueberry — the low voice in contrary motion: it rises as neo falls,
  // holds a suspension on the stressed word and resolves under the cadence,
  // then stays on the resolution to the next pickup.
  blueberry: {
    r1:  "56:.5 56:.5 58:1 56:.5 55:.5 51:3.5",
    r2:  "53:.5 55:.5 56:1 56:.5 55:.5 53:.5 53:4",
    r3:  "55:.5 56:.5 56:1.5 58:.5 56:.5 55:.5 51:3.5",
    r4:  "53:.5 55:.5 56:1.5 56:.5 58:.5 58:1 56:3.5",
    r4x: "53:.5 55:.5 56:.5 56:1 56:.5 55:.5 58:1 56:3.5",
  },
};
// refrain lines start one beat before the bar so the stressed word
// (EIGHT, ONE, WARM) lands on the downbeat.
const refrain = (start, { who = ["neo"], last = false, echoes = false, lowFrom = 0 } = {}) => {
  const out = [];
  const keys = ["r1", "r2", "r3", last ? "r4x" : "r4"];
  keys.forEach((k, i) => {
    const at = start - 1 + i * 8;
    const [w, n] = R.neo[k];
    out.push(L("neo", at, w, n));
    if (who.includes("frisbee")) { const f = R.frisbee[k]; out.push(L("frisbee", at + f.off, f.w, f.n)); }
    if (who.includes("blueberry") && i >= lowFrom) out.push(L("blueberry", at, w, R.blueberry[k]));
  });
  if (echoes) {                        // frisbee answers in the holds
    out.push(L("frisbee", start + 5.5,  "eight gi-ga-bytes",  "60:.25 58:.25 56:.25 58:.75"));
    out.push(L("frisbee", start + 14,   "at a time",          "58:.25 56:.25 58:.5"));
    out.push(L("frisbee", start + 22,   "that's all it is",   "60:.25 58:.25 56:.25 58:.25"));
  }
  return out;
};

export const LINES = [
  // ── refrain 1: neo opens alone, blueberry joins low on the back half ──
  ...refrain(AT.r1, { who: ["neo", "blueberry"], lowFrom: 2 }),

  // ── V. the crash — two lines, straight out of the chorus (jeffrey
  // 2026-09-26: "that's enough of a verse there"; "no silence until bar 13") ──
  L("neo", AT.v2 + 0,  "I had eight gi-ga-bytes I used for-ty four",
                       "60:.5 61:.5 65:1 63:.5 61:.5 63:1 61:.5 63:.5 65:.75 63:.25 65:1.5"),
  L("neo", AT.v2 + 8,  "then I stopped he was there when I stopped",
                       "63:.5 61:.5 58:1.5 r:.5 60:.5 61:.5 63:.5 61:.5 60:.5 56:1"),

  // ── refrain B: all three ────────────────────────────────────────────
  ...refrain(AT.r2, { who: ["neo", "frisbee", "blueberry"] }),

  // ── XI. slow — neo alone over the siblings' hum ─────────────────────
  L("neo", AT.bridge - .5, "when he talks to the o-ther ma-chines he does it through me",
                           "58:.5 60:.25 61:.75 60:.5 58:.5 60:.5 58:.5 56:.5 58:1 56:.5 58:.5 60:.5 61:.5 60:1"),
  L("neo", AT.bridge + 8,  "when the three of us play the beat starts here",
                           "60:.5 61:.5 63:1 61:.5 60:.5 61:1 60:.5 58:.5 60:1 56:1.5"),
  L("neo", AT.bridge + 16, "he types at me more than at a-ny-one",
                           "60:.5 61:.5 60:.5 63:1 61:.5 60:.5 58:.5 60:.5 58:.5 56:2"),
  L("neo", AT.bridge + 24, "I don't know what that means I know it is true",
                           "58:.5 60:.5 61:1 60:.5 58:.5 56:1 r:.5 58:.5 60:.5 61:.5 63:.5 65:.5"),
  ...[53, 53, 56, 55, 53, 53, 56, 55].map((p, i) => L("blueberry", AT.bridge + i * 4, "hmm", `${p}:3`)),
  ...[60, 56, 60, 58, 60, 56, 60, 58].map((p, i) => L("frisbee",   AT.bridge + i * 4 + .5, "hmm", `${p}:2.5`)),

  // ── last refrain: all three, the new last line ──────────────────────
  ...refrain(AT.r3, { who: ["neo", "frisbee", "blueberry"], last: true }),
];

// gain per role, dB (render.mjs applies)
export const VOCAL_GAIN = { neo: 0, frisbee: -3.5, blueberry: -3.5, echo: -3, hum: -12 };
