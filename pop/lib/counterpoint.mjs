// counterpoint.mjs — first-species counterpoint after Fux's
// "Gradus ad Parnassum" (note-against-note, two voices).
//
//   checkFirstSpecies(cantus, counter, opts) -> { ok, violations, warnings }
//   generateFirstSpecies(cantus, opts)       -> counter[] | null
//
// All pitches are MIDI note numbers. The counterpoint sits ABOVE the
// cantus firmus. `cantus` and `counter` are equal-length arrays, one
// counter note per cantus note.
//
// ref: archive.org/details/imslp-ad-parnassum-fux-johann-joseph

// Harmonic-interval classes (semitones mod octave).
const CONSONANT = new Set([0, 3, 4, 7, 8, 9]); // P1 m3 M3 P5 m6 M6
const PERFECT   = new Set([0, 7]);             // P1/P8 and P5

const ic = (semis) => ((semis % 12) + 12) % 12;
export const isConsonant = (semis) => CONSONANT.has(ic(semis));
export const isPerfect   = (semis) => PERFECT.has(ic(semis));

// ── checker ───────────────────────────────────────────────────────────
// Hard rules → `violations`; stylistic preferences → `warnings`.
// `ok` is true when there are no violations.
export function checkFirstSpecies(cantus, counter) {
  const violations = [], warnings = [];
  const n = Math.min(cantus.length, counter.length);
  if (cantus.length !== counter.length)
    violations.push({ index: -1, rule: "length", detail: "voices differ in length" });

  const V = (index, rule, detail) => violations.push({ index, rule, detail });
  const W = (index, rule, detail) => warnings.push({ index, rule, detail });

  let parallelImperfect = 0;
  for (let i = 0; i < n; i++) {
    const iv = counter[i] - cantus[i];

    if (iv < 0)        V(i, "voice-crossing", "counterpoint falls below the cantus");
    else if (iv === 0 && i !== 0 && i !== n - 1)
      W(i, "unison", "interior unison — weak");

    if (!isConsonant(iv))
      V(i, "dissonance", `dissonant interval (${iv} st) — first species is all consonances`);

    if ((i === 0 || i === n - 1) && !isPerfect(iv))
      V(i, "imperfect-bound", `${i === 0 ? "first" : "last"} interval must be a perfect consonance`);

    if (i > 0) {
      const prev = counter[i - 1] - cantus[i - 1];
      const cantusMoved  = cantus[i] !== cantus[i - 1];
      const counterMoved = counter[i] !== counter[i - 1];
      const dirC = Math.sign(cantus[i] - cantus[i - 1]);
      const dirP = Math.sign(counter[i] - counter[i - 1]);

      // parallel perfect consonances
      if (isPerfect(iv) && isPerfect(prev) && ic(iv) === ic(prev) && (cantusMoved || counterMoved))
        V(i, "parallel-perfect", `parallel ${ic(iv) === 0 ? "octaves/unisons" : "fifths"}`);

      // hidden/direct perfect — similar motion into a perfect interval
      if (isPerfect(iv) && !isPerfect(prev) && dirC !== 0 && dirC === dirP)
        W(i, "direct-perfect", "perfect consonance approached by similar motion");

      // melodic leaps in the counterpoint
      const leap = Math.abs(counter[i] - counter[i - 1]);
      if (leap > 12) V(i, "leap", `counterpoint leaps ${leap} st (> octave)`);
      else if (leap > 7) W(i, "wide-leap", `counterpoint leaps ${leap} st`);

      // run of parallel imperfect consonances
      if (!isPerfect(iv) && isConsonant(iv) && ic(iv) === ic(prev) && cantusMoved) {
        parallelImperfect++;
        if (parallelImperfect >= 4)
          W(i, "parallel-run", "4+ parallel thirds/sixths in a row");
      } else parallelImperfect = 0;
    }
  }
  return { ok: violations.length === 0, violations, warnings };
}

// ── generator ─────────────────────────────────────────────────────────
// Backtracking search for a valid first-species line above `cantus`.
// Candidates are notes of the given key/scale within a range above each
// cantus note. Returns the counterpoint array, or null if none found.
//
// opts: {
//   tonic:  MIDI pitch class 0..11   (default = cantus[0] % 12)
//   scale:  semitone degrees         (default natural minor)
//   minAbove, maxAbove: search band over each cantus note (default 2 .. 16)
// }
export function generateFirstSpecies(cantus, opts = {}) {
  const n = cantus.length;
  if (n === 0) return [];
  const tonic = opts.tonic ?? (cantus[0] % 12);
  const scale = opts.scale ?? [0, 2, 3, 5, 7, 8, 10]; // natural minor
  const scaleSet = new Set(scale.map((d) => (d + tonic) % 12));
  const minAbove = opts.minAbove ?? 2;
  const maxAbove = opts.maxAbove ?? 16;

  // candidate pitches for position i, ordered to prefer imperfect
  // consonances and contrary motion vs the previous note.
  const candidatesFor = (i, prev) => {
    const cands = [];
    for (let p = cantus[i] + minAbove; p <= cantus[i] + maxAbove; p++) {
      if (!scaleSet.has(p % 12)) continue;
      const iv = p - cantus[i];
      if (!isConsonant(iv)) continue;
      if (i === 0 || i === n - 1) { if (!isPerfect(iv)) continue; }
      cands.push(p);
    }
    return cands.sort((a, b) => {
      const score = (p) => {
        let s = 0;
        if (isPerfect(p - cantus[i])) s += 3;            // prefer imperfect inside
        if (prev != null) {
          const dirC = Math.sign(cantus[i] - cantus[i - 1]);
          const dirP = Math.sign(p - prev);
          if (dirC !== 0 && dirP === -dirC) s -= 2;       // reward contrary motion
          s += Math.abs(p - prev) * 0.3;                  // reward small steps
        }
        return s;
      };
      return score(a) - score(b);
    });
  };

  const counter = new Array(n);
  const solve = (i) => {
    if (i === n) return checkFirstSpecies(cantus, counter).ok;
    for (const p of candidatesFor(i, i > 0 ? counter[i - 1] : null)) {
      counter[i] = p;
      // prune: the partial line must already be violation-free
      const partial = checkFirstSpecies(cantus.slice(0, i + 1), counter.slice(0, i + 1));
      const onlyBoundary = partial.violations.every(
        (v) => v.rule === "imperfect-bound" && v.index === i && i !== n - 1,
      );
      if (partial.ok || onlyBoundary) {
        if (solve(i + 1)) return true;
      }
    }
    counter[i] = undefined;
    return false;
  };

  return solve(0) ? counter : null;
}
