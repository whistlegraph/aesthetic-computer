// necklace.mjs — the geometry of musical rhythm, as one shared library.
//
// Every function here is specified by a digest entry in papers/rhythm-platter/,
// and the entry is named in the comment above it. If a definition here and the
// digest disagree, the digest is the spec and this file is the bug.
//
// Prior to this module the same mathematics lived in three hand-rolled copies —
// pop/minitek/c/hypnotek.c, dubtek.c, and acidtek.c. Those engines are the
// acceptance test: tests/pop-necklace.test.mjs reproduces their published
// numbers exactly, so lifting the math here is provably lossless.
//
// A rhythm is k onsets on n equally-spaced pulses of a cycle. Pass one as a box
// string ("x..x..x."), a binary string, an onset array with n, or {ioi}. Nearly
// everything takes and returns the normalized form from rhythm().
//
// NOT IMPLEMENTED, deliberately, rather than guessed at: Keith's syncopation
// measure, WNBD as Gómez et al. define it, phylogenetic trees, and Vuza canons.
// Each is named in the digest; none is approximated here.

// ── 01 · representation ─────────────────────────────────────────────────────

/** Normalize any accepted spec into {n, k, onsets, bits}. */
export function rhythm(spec, n) {
  let bits;
  if (typeof spec === "string") {
    const s = spec.trim();
    if (/^[x.\s|]+$/i.test(s)) bits = s.replace(/[\s|]/g, "").split("").map((c) => (c.toLowerCase() === "x" ? 1 : 0));
    else if (/^[01\s|]+$/.test(s)) bits = s.replace(/[\s|]/g, "").split("").map(Number);
    else throw new Error(`unparseable rhythm string: ${spec}`);
  } else if (Array.isArray(spec)) {
    if (n == null) throw new Error("onset array needs an explicit n");
    bits = Array(n).fill(0);
    for (const o of spec) {
      if (!Number.isInteger(o) || o < 0 || o >= n) throw new Error(`onset ${o} outside 0..${n - 1}`);
      bits[o] = 1;
    }
  } else if (spec && Array.isArray(spec.ioi)) {
    bits = fromIoi(spec.ioi).bits;
  } else if (spec && spec.bits) {
    bits = spec.bits.slice();
  } else throw new Error("rhythm(): unrecognized spec");

  if (n != null && bits.length !== n) throw new Error(`length ${bits.length} != n ${n}`);
  const onsets = bits.map((v, i) => (v ? i : -1)).filter((i) => i >= 0);
  return { n: bits.length, k: onsets.length, onsets, bits };
}

/** Build a rhythm from its cyclic inter-onset intervals. Asserts they sum to n. */
export function fromIoi(ioi) {
  const n = ioi.reduce((a, b) => a + b, 0);
  const bits = Array(n).fill(0);
  let p = 0;
  for (const g of ioi) {
    if (g < 1) throw new Error(`non-positive IOI ${g}`);
    bits[p] = 1;
    p += g;
  }
  if (p !== n) throw new Error(`IOIs sum to ${p}, expected ${n}`);
  return rhythm({ bits });
}

/** Cyclic inter-onset intervals. The last one wraps back to the first onset. */
export function toIoi(r) {
  const { onsets: o, n } = rhythm(r);
  if (!o.length) return [];
  return o.map((v, i) => (i + 1 < o.length ? o[i + 1] - v : n - v + o[0]));
}

export const toBox = (r) => rhythm(r).bits.map((v) => (v ? "x" : ".")).join("");
export const toBinary = (r) => rhythm(r).bits.join("");
export const toOnsets = (r) => rhythm(r).onsets;

/** Cyclic shift. Positive rot moves the pattern later in the cycle. */
export function rotate(r, rot) {
  const { bits, n } = rhythm(r);
  const s = ((rot % n) + n) % n;
  return rhythm({ bits: bits.map((_, i) => bits[(i - s + n) % n]) });
}

/** Reverse the cycle about a chosen pulse. The bracelet operation. */
export function reflect(r, axis = 0) {
  const { bits, n } = rhythm(r);
  return rhythm({ bits: bits.map((_, i) => bits[(((axis * 2 - i) % n) + n) % n]) });
}

/** Lexicographically least rotation — the necklace representative. */
export function necklaceCanonical(r) {
  const { bits, n } = rhythm(r);
  let best = null;
  for (let s = 0; s < n; s++) {
    const t = bits.slice(s).concat(bits.slice(0, s)).join("");
    if (best === null || t < best) best = t;
  }
  return best;
}

/** Least over rotations of the rhythm AND its reversal — the bracelet representative. */
export function braceletCanonical(r) {
  const f = necklaceCanonical(r);
  const b = necklaceCanonical(reflect(r));
  return f < b ? f : b;
}

export const sameNecklace = (a, b) => necklaceCanonical(a) === necklaceCanonical(b);
export const sameBracelet = (a, b) => braceletCanonical(a) === braceletCanonical(b);

const gcd = (a, b) => { while (b) { [a, b] = [b, a % b]; } return a; };
const totient = (m) => { let r = m; for (let p = 2; p * p <= m; p++) if (m % p === 0) { while (m % p === 0) m /= p; r -= r / p; } if (m > 1) r -= r / m; return r; };
const binom = (n, k) => { if (k < 0 || k > n) return 0; let r = 1; for (let i = 0; i < k; i++) r = (r * (n - i)) / (i + 1); return Math.round(r); };

/** N(n,k) = (1/n) Σ_{d | gcd(n,k)} φ(d) · C(n/d, k/d). */
export function necklaceCount(n, k) {
  let sum = 0;
  const g = gcd(n, k);
  for (let d = 1; d <= g; d++) if (g % d === 0) sum += totient(d) * binom(n / d, k / d);
  return sum / n;
}

/**
 * All necklace representatives with k onsets on n pulses.
 * Enumerated, not closed-form — see digest/01 on why bracelets get no formula.
 */
export function enumerateNecklaces(n, k, { limit = 2e6 } = {}) {
  const total = binom(n, k);
  if (total > limit) throw new Error(`C(${n},${k}) = ${total} exceeds limit ${limit}`);
  const seen = new Set();
  const combo = [];
  (function walk(start) {
    if (combo.length === k) { seen.add(necklaceCanonical(rhythm(combo, n))); return; }
    for (let i = start; i < n; i++) { combo.push(i); walk(i + 1); combo.pop(); }
  })(0);
  return [...seen].sort();
}

/** Bracelet representatives. Enumeration only — the closed form splits on parity. */
export function enumerateBracelets(n, k, opts) {
  return [...new Set(enumerateNecklaces(n, k, opts).map((s) => braceletCanonical(s)))].sort();
}

export const braceletCount = (n, k, opts) => enumerateBracelets(n, k, opts).length;

// ── 02 · evenness ───────────────────────────────────────────────────────────

/**
 * Bjorklund's algorithm. Grow k [1] groups and (n-k) [0] groups, fold the
 * smaller pile onto the larger until one pile has at most one group, concatenate.
 * Verified against all 48 entries of Toussaint (2005) §4 by the platter build.
 */
export function bjorklund(k, n, rot = 0) {
  if (n <= 0) throw new Error("n must be positive");
  if (k <= 0) return rhythm({ bits: Array(n).fill(0) });
  if (k >= n) return rhythm(Array.from({ length: n }, (_, i) => i), n);
  let a = Array.from({ length: k }, () => [1]);
  let b = Array.from({ length: n - k }, () => [0]);
  while (b.length > 1) {
    const m = Math.min(a.length, b.length);
    const na = [], nb = [];
    for (let i = 0; i < m; i++) na.push(a[i].concat(b[i]));
    const rest = a.length > m ? a : b;
    for (let i = m; i < rest.length; i++) nb.push(rest[i]);
    a = na; b = nb;
  }
  const bits = a.concat(b).flat();
  return rot ? rotate(rhythm({ bits }), rot) : rhythm({ bits });
}

/**
 * Exact: is this rhythm the maximally even set for its (k, n)?
 * The two-gap property is necessary but NOT sufficient — it constrains the gap
 * multiset and not its order — so this compares necklaces. See digest/02.
 */
export function isMaximallyEven(r) {
  const { k, n } = rhythm(r);
  if (k < 2) return true;
  return sameNecklace(r, bjorklund(k, n));
}

/** The two distinct gap sizes and their counts. A fast reject, not a decision. */
export function gapClasses(r) {
  const ioi = toIoi(r);
  const counts = new Map();
  for (const g of ioi) counts.set(g, (counts.get(g) ?? 0) + 1);
  const sizes = [...counts.keys()].sort((x, y) => x - y);
  return { sizes, counts: Object.fromEntries(counts), twoGap: sizes.length <= 2 && (sizes.length < 2 || sizes[1] - sizes[0] === 1) };
}

/**
 * Sum of pairwise Euclidean CHORD lengths, onsets as points on the unit circle.
 * Demaine et al. (2009) prove the maximally even set maximizes exactly this, so
 * chord-sum is the definition. Arc-sum is a different, wrong functional.
 */
export function evennessChordSum(r) {
  const { onsets: o, n } = rhythm(r);
  let sum = 0;
  for (let a = 0; a < o.length; a++)
    for (let b = a + 1; b < o.length; b++) sum += 2 * Math.abs(Math.sin((Math.PI * (o[a] - o[b])) / n));
  return sum;
}

/** Chord-sum normalized by the regular k-gon's. 1.0 == maximally even. */
export function evenness(r) {
  const { k } = rhythm(r);
  if (k < 2) return 1;
  let ideal = 0;
  for (let a = 0; a < k; a++)
    for (let b = a + 1; b < k; b++) ideal += 2 * Math.abs(Math.sin((Math.PI * (a - b)) / k));
  return ideal > 0 ? evennessChordSum(r) / ideal : 1;
}

/**
 * AC-LOCAL, not Toussaint: 1 - meanAbsDev/worstDev over the IOIs, where worst
 * case is all onsets adjacent. Lifted verbatim from hypnotek.c so its published
 * numbers reproduce. Prefer evenness() for anything new.
 */
export function evennessIoi(r) {
  const { k, n } = rhythm(r);
  if (k < 2) return 1;
  const ideal = n / k;
  const ioi = toIoi(r);
  const meanAbsDev = ioi.reduce((s, g) => s + Math.abs(g - ideal), 0) / k;
  const worstDev = ((k - 1) * Math.abs(1 - ideal) + Math.abs(n - (k - 1) - ideal)) / k;
  return worstDev > 1e-9 ? 1 - meanAbsDev / worstDev : 1;
}

/** Variance of the cyclic IOIs. Lower = more even. */
export function ioiVariance(r) {
  const ioi = toIoi(r);
  if (!ioi.length) return 0;
  const mean = ioi.reduce((a, b) => a + b, 0) / ioi.length;
  return ioi.reduce((s, g) => s + (g - mean) ** 2, 0) / ioi.length;
}

/**
 * AC-LOCAL, not Toussaint: summed distance from each onset to the nearest
 * vertex of a best-fit ideal k-gon, minimized over a sub-step phase sweep.
 * Lifted verbatim from hypnotek.c, phase sweep included, so D reproduces.
 */
export function vertexDistance(r) {
  const { onsets: o, k, n } = rhythm(r);
  if (k < 2) return 0;
  const ideal = n / k;
  let best = Infinity;
  for (let ph = 0; ph < n; ph++) {
    let sum = 0;
    for (const pos of o) {
      let bestv = Infinity;
      for (let j = 0; j < k; j++) {
        const v = j * ideal + (ph * ideal) / n;
        let d = Math.abs(pos - v);
        if (d > n / 2) d = n - d;
        if (d < bestv) bestv = d;
      }
      sum += bestv;
    }
    if (sum < best) best = sum;
  }
  return best;
}

/** DFT of the onset indicator. Shared backend for balance and evenness. */
export function dft(r) {
  const { onsets: o, n } = rhythm(r);
  return Array.from({ length: n }, (_, j) => {
    let re = 0, im = 0;
    for (const p of o) { const a = (-2 * Math.PI * j * p) / n; re += Math.cos(a); im += Math.sin(a); }
    return { re, im, mag: Math.hypot(re, im) };
  });
}

/**
 * Centre of mass of the onsets as unit vectors. magnitude 0 == perfectly
 * balanced. Distinct from maximal evenness — see digest/02. Under the spatial
 * mapping the angle IS the sound field's centroid direction.
 */
export function balance(r) {
  const { onsets: o, n, k } = rhythm(r);
  let re = 0, im = 0;
  for (const p of o) { const a = (2 * Math.PI * p) / n; re += Math.cos(a); im += Math.sin(a); }
  const magnitude = Math.hypot(re, im);
  // A perfectly balanced rhythm has NO direction. Without this guard atan2
  // returns the angle of the floating-point residue, and the spatial layer
  // steers by that angle — so the garbage would become an audible heading.
  const balanced = magnitude < 1e-9;
  return { magnitude, normalized: k ? magnitude / k : 0, angle: balanced ? 0 : Math.atan2(im, re), balanced };
}

/**
 * The generating interval, if the onset set is {0, g, 2g, ...} up to rotation.
 * This is "generated", which is necessary for Carey–Clampitt well-formedness but
 * not the whole of it; the ordering condition is not tested here.
 */
export function generatedBy(r) {
  const { k, n } = rhythm(r);
  if (k < 2) return null;
  for (let g = 1; g < n; g++) {
    const gen = rhythm([...new Set(Array.from({ length: k }, (_, i) => (i * g) % n))], n);
    if (gen.k === k && sameNecklace(gen, r)) return g;
  }
  return null;
}

// ── 03 · oddity, depth, interval content ────────────────────────────────────

/**
 * Arom's rhythmic oddity: no two onsets divide the cycle into equal halves.
 * Returns null for odd n, where the question is vacuous — a deliberate
 * difference from dubtek.c, which returned true. See digest/03.
 */
export function hasRhythmicOddity(r) {
  const { bits, n } = rhythm(r);
  if (n % 2) return null;
  const half = n / 2;
  for (let p = 0; p < n; p++) if (bits[p] && bits[(p + half) % n]) return false;
  return true;
}

/** Histogram of geodesic distances over all C(k,2) onset pairs. Not the IOIs. */
export function intervalVector(r) {
  const { onsets: o, n } = rhythm(r);
  const v = Array(Math.floor(n / 2) + 1).fill(0);
  for (let a = 0; a < o.length; a++)
    for (let b = a + 1; b < o.length; b++) {
      let d = Math.abs(o[a] - o[b]);
      if (d > n / 2) d = n - d;
      v[d]++;
    }
  return v;
}

/** Every distance 1..floor(n/2) has a unique multiplicity. */
export function isWinogradDeep(r) {
  const v = intervalVector(r).slice(1);
  return new Set(v).size === v.length;
}

/** For every multiplicity 1..k-1 some distance occurs exactly that many times. */
export function isErdosDeep(r) {
  const { k } = rhythm(r);
  if (k < 2) return true;
  const v = intervalVector(r).slice(1);
  for (let m = 1; m <= k - 1; m++) if (!v.includes(m)) return false;
  return true;
}

/**
 * An ordering of onsets whose successive removal leaves an Erdős-deep rhythm at
 * every step — a principled thinning. Returns null if no shelling exists.
 * The breakdown/build lever from digest/03.
 */
export function shelling(r) {
  const base = rhythm(r);
  if (!isErdosDeep(base)) return null;
  const order = [];
  const walk = (onsets) => {
    if (onsets.length <= 2) return true;
    for (const o of onsets) {
      const rest = onsets.filter((x) => x !== o);
      if (isErdosDeep(rhythm(rest, base.n))) { order.push(o); if (walk(rest)) return true; order.pop(); }
    }
    return false;
  };
  return walk(base.onsets) ? order : null;
}

/** Distinct necklaces with identical interval vectors. */
export function homometricPairs(n, k, opts) {
  const byVector = new Map();
  for (const s of enumerateNecklaces(n, k, opts)) {
    const key = intervalVector(s).join(",");
    if (!byVector.has(key)) byVector.set(key, []);
    byVector.get(key).push(s);
  }
  const pairs = [];
  for (const group of byVector.values())
    if (group.length > 1)
      for (let a = 0; a < group.length; a++) for (let b = a + 1; b < group.length; b++) pairs.push([group[a], group[b]]);
  return pairs;
}

/** The generators of C(n): pulses p with gcd(p, n) == 1. The off-beat positions. */
export const generators = (n) => Array.from({ length: n }, (_, p) => p).filter((p) => gcd(p, n) === 1);

/**
 * Toussaint's off-beatness: onsets on group-generator pulses. Degenerates for
 * prime n, where every nonzero pulse is a generator — flagged, not hidden.
 * The one measure here that is NOT rotation-invariant.
 */
export function offbeatness(r) {
  const { bits, n } = rhythm(r);
  const gens = generators(n);
  const positions = gens.filter((p) => bits[p]);
  const isPrime = n > 1 && Array.from({ length: n - 2 }, (_, i) => i + 2).every((d) => d * d > n || n % d);
  return { count: positions.length, positions, degenerate: isPrime };
}

// ── 04 · syncopation ────────────────────────────────────────────────────────

/**
 * Metric weights by recursive binary subdivision, strongest at the downbeat.
 * An INPUT, not a derived fact: a different metric reading gives different
 * syncopation numbers for the same onsets. See digest/04.
 */
export function metricWeights(n, subdivision = 2) {
  const spans = [];
  for (let s = n; s >= 1; s /= subdivision) {
    if (!Number.isInteger(s)) break;
    spans.push(s);
  }
  // A pulse's weight is how many levels of the hierarchy land on it: the
  // downbeat sits on every span, an odd 16th on only the finest.
  return Array.from({ length: n }, (_, p) => spans.filter((s) => p % s === 0).length);
}

/**
 * Longuet-Higgins & Lee: a rest at a stronger metric position than the note
 * sounding through it contributes the level difference. Discrete form.
 */
export function syncopationLhl(r, weights) {
  const { bits, n } = rhythm(r);
  const w = weights ?? metricWeights(n);
  let total = 0, last = -1;
  for (let p = 0; p < n; p++) if (bits[p]) { last = p; break; }
  if (last < 0) return 0;
  let sounding = last;
  for (let i = 0; i < n; i++) {
    const p = (last + i) % n;
    if (bits[p]) sounding = p;
    else if (w[p] > w[sounding]) total += w[p] - w[sounding];
  }
  return total;
}

/**
 * Povel & Essens C-score: counterevidence of the best-fitting induced clock.
 * Accents follow their rules (isolated onset; second of a pair; first and last
 * of a run of three or more). W weights a clock tick landing on silence.
 */
export function syncopationPovelEssens(r, { W = 4 } = {}) {
  const { bits, n } = rhythm(r);
  const accent = Array(n).fill(false);
  const runs = [];
  let i = 0;
  while (i < n) {
    if (!bits[i]) { i++; continue; }
    let j = i;
    while (j < n && bits[j]) j++;
    runs.push([i, j - 1]);
    i = j;
  }
  for (const [a, b] of runs) {
    const len = b - a + 1;
    if (len === 1) accent[a] = true;
    else if (len === 2) accent[b] = true;
    else { accent[a] = true; accent[b] = true; }
  }
  let best = Infinity, bestClock = null;
  // A clock needs at least two ticks inside the cycle. Without this bound the
  // period-n "clock" has a single tick, lands on any onset, and scores a
  // meaningless zero for every pattern that starts on the downbeat.
  for (let period = 1; period <= n / 2; period++) {
    if (n % period) continue;
    for (let phase = 0; phase < period; phase++) {
      let c = 0;
      for (let p = phase; p < n; p += period) {
        if (!bits[p]) c += W;
        else if (!accent[p]) c += 1;
      }
      if (c < best) { best = c; bestClock = { period, phase }; }
    }
  }
  return { cScore: best, bestClock };
}

/** Mean distance from each onset to the nearest beat. Not Gómez et al.'s WNBD. */
export function noteToBeatDistance(r, beats) {
  const { onsets: o, n } = rhythm(r);
  const b = beats ?? Array.from({ length: 4 }, (_, i) => (i * n) / 4);
  if (!o.length) return 0;
  return o.reduce((s, p) => s + Math.min(...b.map((q) => { let d = Math.abs(p - q); return d > n / 2 ? n - d : d; })), 0) / o.length;
}

/** The vector engines should print, rather than one number pretending to decide. */
export function syncopationProfile(r) {
  return {
    lhl: syncopationLhl(r),
    cScore: syncopationPovelEssens(r).cScore,
    noteToBeat: noteToBeatDistance(r),
    offbeatness: offbeatness(r),
  };
}

// ── 05 · distance and similarity ────────────────────────────────────────────

/** Positions differing. O(n) and inappropriate for rhythm — see digest/05. */
export function distHamming(a, b) {
  const x = rhythm(a), y = rhythm(b);
  if (x.n !== y.n) throw new Error("Hamming needs equal n");
  let d = 0;
  for (let i = 0; i < x.n; i++) if (x.bits[i] !== y.bits[i]) d++;
  return d;
}

/** Minimum adjacent swaps. For equal k this is the L1 distance of sorted onsets. */
export function distSwap(a, b) {
  const x = rhythm(a), y = rhythm(b);
  if (x.n !== y.n) throw new Error("swap needs equal n");
  if (x.k !== y.k) throw new Error("swap needs equal k — use distDirectedSwap");
  return x.onsets.reduce((s, p, i) => s + Math.abs(p - y.onsets[i]), 0);
}

/** Asymmetric: every onset of a moves to the nearest onset of b. */
export function distDirectedSwap(a, b) {
  const x = rhythm(a), y = rhythm(b);
  if (!y.k) throw new Error("target has no onsets");
  return x.onsets.reduce((s, p) => s + Math.min(...y.onsets.map((q) => { let d = Math.abs(p - q); return d > x.n / 2 ? x.n - d : d; })), 0);
}

/** Per-pulse height = the IOI that pulse falls inside. Gustafson / Hofmann-Engl. */
export function chronotonicVector(r) {
  const { onsets: o, n } = rhythm(r);
  if (!o.length) return Array(n).fill(0);
  const ioi = toIoi(r);
  const v = Array(n).fill(0);
  o.forEach((start, i) => { for (let d = 0; d < ioi[i]; d++) v[(start + d) % n] = ioi[i]; });
  return v;
}

/**
 * Area between two chronotonic curves (Kolmogorov variational distance).
 * Toussaint (2004) rates this the best overall measure — so it is the default.
 * Cycles of different length are resampled to their lcm.
 */
export function distChronotonic(a, b) {
  const x = rhythm(a), y = rhythm(b);
  const L = (x.n * y.n) / gcd(x.n, y.n);
  const up = (r) => { const v = chronotonicVector(r); const f = L / rhythm(r).n; return Array.from({ length: L }, (_, i) => (v[Math.floor(i / f)] * f)); };
  const u = up(x), w = up(y);
  return u.reduce((s, val, i) => s + Math.abs(val - w[i]), 0) / L;
}

/** L1 between full interval vectors. Blind to homometric pairs by construction. */
export function distInterval(a, b) {
  const u = intervalVector(a), v = intervalVector(b);
  const len = Math.max(u.length, v.length);
  let d = 0;
  for (let i = 0; i < len; i++) d += Math.abs((u[i] ?? 0) - (v[i] ?? 0));
  return d;
}

const MEASURES = { chronotonic: distChronotonic, swap: distSwap, directedSwap: distDirectedSwap, hamming: distHamming, interval: distInterval };

/**
 * Rhythms are cyclic, so the distance between NECKLACES minimizes over
 * rotations. The minimizing rotation is itself useful — it is the phase at which
 * two patterns are most alike, and therefore a defensible place to cut.
 */
export function dist(a, b, { measure = "chronotonic", cyclic = true } = {}) {
  const fn = MEASURES[measure];
  if (!fn) throw new Error(`unknown measure ${measure}`);
  const x = rhythm(a), y = rhythm(b);
  if (!cyclic) return { distance: fn(x, y), rotation: 0 };
  let best = Infinity, bestRot = 0;
  for (let s = 0; s < y.n; s++) {
    let d;
    try { d = fn(x, rotate(y, s)); } catch { continue; }
    if (d < best) { best = d; bestRot = s; }
  }
  return { distance: best, rotation: bestRot };
}

/**
 * The geodesic from a to b: one onset moves one pulse per step. An arrangement
 * device AC has not had — the groove becomes the other groove while you listen.
 */
export function morphPath(a, b, { align = true } = {}) {
  const x = rhythm(a);
  let y = rhythm(b);
  if (x.n !== y.n) throw new Error("morph needs equal n");
  if (x.k !== y.k) throw new Error("morph needs equal k");
  if (align) y = rotate(y, dist(x, y, { measure: "swap" }).rotation);
  const path = [x];
  const cur = x.onsets.slice();
  const target = y.onsets.slice();
  let guard = 0;
  while (cur.some((p, i) => p !== target[i]) && guard++ < x.n * x.k) {
    // Move any onset one pulse toward its target, but only into a free cell —
    // otherwise two onsets collide and the rhythm silently loses a beat.
    const i = cur.findIndex((p, j) => {
      if (p === target[j]) return false;
      const next = p + Math.sign(target[j] - p);
      return next >= 0 && next < x.n && !cur.includes(next);
    });
    if (i < 0) break;
    cur[i] += Math.sign(target[i] - cur[i]);
    path.push(rhythm(cur.slice().sort((a2, b2) => a2 - b2), x.n));
  }
  return path;
}

// ── 06 · complements and canons ─────────────────────────────────────────────

export function complement(r) {
  const { bits } = rhythm(r);
  return rhythm({ bits: bits.map((v) => (v ? 0 : 1)) });
}

export const interlocks = (a, b) => { const x = rhythm(a), y = rhythm(b); return x.n === y.n && !x.bits.some((v, i) => v && y.bits[i]); };

export const sameIntervalVector = (a, b) => intervalVector(a).join(",") === intervalVector(b).join(",");

/** Offsets t such that the translates of r tile the cycle exactly once. */
export function tilingPartners(r) {
  const { onsets: o, n, k } = rhythm(r);
  if (!k || n % k) return [];
  const need = n / k;
  const found = [];
  const walk = (chosen, covered) => {
    if (chosen.length === need) { if (covered.size === n) found.push(chosen.slice()); return; }
    for (let t = chosen.length ? chosen[chosen.length - 1] + 1 : 0; t < n; t++) {
      const hits = o.map((p) => (p + t) % n);
      if (hits.some((h) => covered.has(h))) continue;
      const next = new Set(covered);
      hits.forEach((h) => next.add(h));
      chosen.push(t); walk(chosen, next); chosen.pop();
    }
  };
  walk([], new Set());
  return found;
}

export function isTilingCanon(r, translations) {
  const { onsets: o, n } = rhythm(r);
  const covered = new Set();
  for (const t of translations) for (const p of o) { const h = (p + t) % n; if (covered.has(h)) return false; covered.add(h); }
  return covered.size === n;
}

/** True if some proper rotation maps the rhythm onto itself. */
export function isPeriodic(r) {
  const { bits, n } = rhythm(r);
  for (let s = 1; s < n; s++) if (bits.every((v, i) => v === bits[(i + s) % n])) return true;
  return false;
}

// ── the full report ─────────────────────────────────────────────────────────

/** Everything measurable about one rhythm. What engines should print. */
export function analyze(r) {
  const x = rhythm(r);
  return {
    n: x.n, k: x.k, onsets: x.onsets, box: toBox(x), ioi: toIoi(x),
    necklace: necklaceCanonical(x), bracelet: braceletCanonical(x),
    euclidean: isMaximallyEven(x),
    evenness: evenness(x), evennessIoi: evennessIoi(x),
    ioiVariance: ioiVariance(x), vertexDistance: vertexDistance(x),
    balance: balance(x),
    rhythmicOddity: hasRhythmicOddity(x),
    intervalVector: intervalVector(x),
    winogradDeep: isWinogradDeep(x), erdosDeep: isErdosDeep(x),
    offbeatness: offbeatness(x),
    syncopation: syncopationProfile(x),
    periodic: isPeriodic(x),
  };
}
