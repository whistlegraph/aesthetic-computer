// necklace-space.mjs — put a rhythm cycle in the space around a listener's head.
//
// The mapping law from papers/rhythm-platter/digest/08: step i of an n-pulse
// cycle sits at azimuth 2πi/n, step 0 dead ahead, in the horizontal plane.
// Rotation of the necklace becomes rotation of the sound field; reflection
// becomes a left/right mirror across the median plane.
//
// Why the horizontal plane and not some other: bracelet reflection has to land
// on the BEST-resolved axis or the experiment proves nothing. Left/right is
// resolved by interaural time and level differences; front/back and elevation
// sit inside the cone of confusion. Reflecting about the frontal plane instead
// would be a null test dressed up as a real one.
//
// Feeds ac_hrtf.h (pop/nullabye/c/), which takes azimuth, elevation, distance.
// This module decides WHERE; that one decides how it sounds.

import { rhythm, rotate, balance, offbeatness, toIoi } from "./necklace.mjs";

const TAU = Math.PI * 2;
const DEG = 180 / Math.PI;

// ── perceptual limits (digest/08) ───────────────────────────────────────────

/**
 * Mills (1958): the minimum audible angle is about 1° frontally and degrades to
 * 10° or worse toward the sides. At n=16 the beads are 22.5° apart — clean
 * ahead, marginal at the extreme lateral positions. At n=32 they are 11.25° and
 * the lateral beads stop being distinct places.
 *
 * A hard ceiling on the mapping law, so it warns rather than silently rendering
 * a ring nobody can resolve.
 */
export function minAudibleAngleCheck(n) {
  const spacing = 360 / n;
  const FRONTAL = 1, LATERAL = 10;
  // Mills gives roughly 10° "or worse" laterally, so a spacing merely above 10°
  // is not safely clear of it. Anything under 2x the figure is called marginal
  // rather than resolvable — n=16 (22.5°) is clean, n=32 (11.25°) is not.
  const lateral = spacing >= LATERAL * 2 ? "resolvable" : spacing >= LATERAL ? "marginal" : "unresolvable";
  return {
    spacingDeg: spacing,
    resolvableFrontally: spacing >= FRONTAL,
    lateral,
    warning: lateral === "resolvable" ? null
      : `n=${n} spaces steps ${spacing.toFixed(2)}° apart; against Mills' ~${LATERAL}°-or-worse lateral minimum audible angle, beads near ±90° are ${lateral === "marginal" ? "marginal and may not read as separate places" : "not separately localisable"}`,
  };
}

/**
 * Wallach et al. (1949): onsets fusing within roughly 1–5 ms are localised to
 * the first arrival. At 120 BPM a 16th step is 125 ms, far clear — but a fast
 * lane or a large n can cross it, at which point two beads become one place.
 */
export function precedenceCheck(n, bpm, { beatsPerCycle = 4 } = {}) {
  const cycleMs = (60000 / bpm) * beatsPerCycle;
  const stepMs = cycleMs / n;
  const FUSION_MS = 5;
  return {
    cycleMs, stepMs,
    fuses: stepMs < FUSION_MS,
    warning: stepMs < FUSION_MS
      ? `steps are ${stepMs.toFixed(2)} ms apart, inside the ~${FUSION_MS} ms precedence window; adjacent beads will localise to one point`
      : null,
  };
}

/**
 * London (2012): a felt beat lives roughly between 100 ms and 2 s. Outside that
 * window a period is countable but not felt, so the ring stops being heard as a
 * cycle. Called before rendering so a lane cannot silently write one.
 */
export function entrainmentCheck(n, bpm, { beatsPerCycle = 4 } = {}) {
  const cycleMs = (60000 / bpm) * beatsPerCycle;
  const pulseMs = cycleMs / n;
  const warnings = [];
  if (cycleMs > 2000) warnings.push(`cycle is ${(cycleMs / 1000).toFixed(2)} s, past the ~2 s ceiling for a felt cycle`);
  if (pulseMs < 100) warnings.push(`pulses are ${pulseMs.toFixed(1)} ms, under the ~100 ms floor for a felt pulse`);
  return { cycleMs, pulseMs, inWindow: warnings.length === 0, warnings };
}

/**
 * The bound past which a listener re-parses rather than tracks, so the identity
 * of the precessing necklace is lost. Derived from the entrainment ceiling: one
 * full revolution should take at least a cycle's worth of felt time.
 */
export function precessionRateLimit(n, bpm, { beatsPerCycle = 4, barsPerPhrase = 4 } = {}) {
  const { cycleMs } = entrainmentCheck(n, bpm, { beatsPerCycle });
  const phraseMs = cycleMs * barsPerPhrase;
  // One revolution per felt-cycle is the fastest that still reads as the same
  // ring turning rather than as a new pattern each phrase.
  const maxRevolutionsPerPhrase = phraseMs / Math.max(cycleMs, 1);
  return {
    maxStepsPerPhrase: Math.floor(maxRevolutionsPerPhrase * n),
    phraseMs,
    note: "beyond this the listener re-parses instead of tracking; see digest/07",
  };
}

// ── the mapping ─────────────────────────────────────────────────────────────

/** Elevation strategies. `flat` is the control condition and ships alongside. */
export const ELEVATION = {
  /** Everything at ear level — the purest test of the azimuth claim. */
  flat: () => 0,
  /**
   * Generator pulses (gcd(p,n)=1) lift above the plane. Off-beatness is the one
   * measure rotation changes, so this is the axis that visibly MOVES as the
   * necklace precesses. The strongest pairing per digest/08.
   */
  offbeat: (step, r, { lift = Math.PI / 8 } = {}) =>
    offbeatness(r).positions.includes(step) ? lift : 0,
  /** Onsets closing a long gap ride higher — makes the two-gap structure visible. */
  gap: (step, r, { lift = Math.PI / 8 } = {}) => {
    const x = rhythm(r);
    const i = x.onsets.indexOf(step);
    if (i < 0) return 0;
    const ioi = toIoi(x);
    const max = Math.max(...ioi);
    const min = Math.min(...ioi);
    return max === min ? 0 : lift * ((ioi[i] - min) / (max - min));
  },
};

/**
 * Place a rhythm's onsets around the listener.
 *
 * Returns one entry per ONSET (not per pulse), each carrying the azimuth its
 * step occupies, so the caller can hand them straight to ac_hrtf.h.
 *
 * @param r          the rhythm
 * @param radius     distance in room units
 * @param elevation  a key of ELEVATION, or a function (step, rhythm) => radians
 * @param rotation   rotate the whole ring — the necklace-equivalence operation
 * @param mirror     reflect across the median plane — the bracelet operation
 * @param clockwise  which way step index advances around the head
 */
export function necklaceToPositions(r, {
  radius = 2,
  elevation = "flat",
  rotation = 0,
  mirror = false,
  clockwise = true,
} = {}) {
  const x = rhythm(r);
  const elevate = typeof elevation === "function" ? elevation : ELEVATION[elevation];
  if (!elevate) throw new Error(`unknown elevation strategy: ${elevation}`);

  const spun = rotation ? rotate(x, rotation) : x;
  return spun.onsets.map((step) => {
    let azimuth = (TAU * step) / x.n;
    if (!clockwise) azimuth = -azimuth;
    if (mirror) azimuth = -azimuth;                       // median-plane reflection
    azimuth = ((azimuth + Math.PI) % TAU + TAU) % TAU - Math.PI; // wrap to (-π, π]
    return {
      step,
      azimuth,
      azimuthDeg: azimuth * DEG,
      elevation: elevate(step, spun),
      distance: radius,
    };
  });
}

/** Mirror an already-placed ring. The A/B operation, applied to positions. */
export const reflectSpatial = (positions) =>
  positions.map((p) => ({ ...p, azimuth: -p.azimuth, azimuthDeg: -p.azimuthDeg }));

/**
 * Where the sound field pulls. Under this mapping the balance vector IS the
 * centroid of the ring, so a perfectly balanced necklace has no heading and an
 * unbalanced one leans into one side of the room.
 *
 * The lane's primary steering signal — more than evenness, per digest/08.
 */
export function balanceDirection(r, { rotation = 0, mirror = false } = {}) {
  const b = balance(rotation ? rotate(rhythm(r), rotation) : r);
  const angle = b.balanced ? 0 : (mirror ? -b.angle : b.angle);
  return { angle, angleDeg: angle * DEG, magnitude: b.magnitude, normalized: b.normalized, balanced: b.balanced };
}

/** Everything a lane should check before it renders a spatial ring. */
export function spatialReport(r, bpm, opts = {}) {
  const x = rhythm(r);
  const maa = minAudibleAngleCheck(x.n);
  const prec = precedenceCheck(x.n, bpm, opts);
  const ent = entrainmentCheck(x.n, bpm, opts);
  return {
    n: x.n, k: x.k, bpm,
    positions: necklaceToPositions(x, opts),
    balance: balanceDirection(x, opts),
    minAudibleAngle: maa,
    precedence: prec,
    entrainment: ent,
    precession: precessionRateLimit(x.n, bpm, opts),
    warnings: [maa.warning, prec.warning, ...ent.warnings].filter(Boolean),
  };
}
