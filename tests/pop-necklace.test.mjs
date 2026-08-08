// pop-necklace.test.mjs — acceptance test for pop/lib/necklace.mjs.
//
// The library exists to replace hand-rolled copies of the same math in
// pop/minitek/c/{hypnotek,dubtek,acidtek}.c. Lifting is only lossless if the
// published numbers reproduce, so those numbers are the test — captured from the
// engines' own stderr, not retyped from the theses.
//
//   node --test tests/pop-necklace.test.mjs

import { test, describe } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

import {
  rhythm, fromIoi, toIoi, toBox, rotate, reflect,
  necklaceCanonical, braceletCanonical, sameNecklace, sameBracelet,
  necklaceCount, enumerateNecklaces, enumerateBracelets,
  bjorklund, isMaximallyEven, gapClasses, evenness, evennessIoi,
  ioiVariance, vertexDistance, balance, generatedBy,
  hasRhythmicOddity, intervalVector, isWinogradDeep, isErdosDeep, shelling,
  homometricPairs, generators, offbeatness,
  metricWeights, syncopationLhl, syncopationPovelEssens,
  distHamming, distSwap, distChronotonic, chronotonicVector, dist, morphPath,
  complement, interlocks, sameIntervalVector, tilingPartners, isTilingCanon, isPeriodic,
  analyze,
} from "../pop/lib/necklace.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const round = (v, d = 3) => Number(v.toFixed(d));

// ── the platter catalogue ───────────────────────────────────────────────────

describe("platter catalogue", () => {
  const platter = JSON.parse(readFileSync(join(HERE, "..", "papers", "rhythm-platter", "timelines.json"), "utf8"));

  test("reproduces all 48 Euclidean rhythms from Toussaint (2005) §4", () => {
    const rows = platter.euclidean_catalogue.rows;
    assert.equal(rows.length, 48);
    for (const row of rows) {
      assert.ok(
        sameNecklace(bjorklund(row.k, row.n), row.box),
        `${row.id}: bjorklund disagrees with the published ${row.box}`,
      );
    }
  });

  test("named timelines carry the recorded Euclidean membership", () => {
    for (const row of platter.named_timelines.rows) {
      assert.equal(isMaximallyEven(rhythm(row.onsets, row.n)), row.euclidean, `${row.id}`);
    }
  });

  test("only bossa is Euclidean among the six 16-pulse claves", () => {
    const claves = platter.named_timelines.rows.filter((r) => r.n === 16);
    assert.deepEqual(claves.filter((r) => r.euclidean).map((r) => r.id), ["bossa"]);
  });

  test("son clave is NOT E(5,16) — the correction the digest found", () => {
    const son = rhythm([0, 3, 6, 10, 12], 16);
    const bossa = rhythm([0, 3, 6, 10, 13], 16);
    assert.equal(sameNecklace(son, bjorklund(5, 16)), false);
    assert.equal(sameNecklace(bossa, bjorklund(5, 16)), true);
    assert.deepEqual(toIoi(son), [3, 3, 4, 2, 4]);
    assert.deepEqual(toIoi(bjorklund(5, 16)), [3, 3, 3, 3, 4]);
  });
});

// ── hypnotek.c EXPERIMENT 1, captured from its stderr ───────────────────────

describe("hypnotek.c parity", () => {
  // lane, k, n, box, IOIvar, E, D — verbatim from `./hypnotek` stderr.
  const LANES = [
    ["E_KICK", 4, 16, "x...x...x...x...", 0.0, 1.0, 0.0],
    ["E_CHAT", 7, 16, "x..x.x.x..x.x.x.", 0.204, 0.815, 1.71],
    ["E_OHAT", 3, 8, "x..x..x.", 0.222, 0.8, 0.67],
    ["E_BLIP", 5, 16, "x..x..x..x..x...", 0.16, 0.909, 1.2],
    ["E_RIM", 3, 16, "x....x....x.....", 0.222, 0.923, 0.67],
    ["E_SHAK", 9, 16, "x.xx.x.x.xx.x.x.", 0.173, 0.75, 2.22],
    ["E_RIDE", 2, 16, "x.......x.......", 0.0, 1.0, 0.0],
    ["E_CBASS", 5, 16, "x..x..x..x..x...", 0.16, 0.909, 1.2],
  ];

  for (const [name, k, n, box, ioiVar, E, D] of LANES) {
    test(`${name} E(${k},${n})`, () => {
      const r = bjorklund(k, n);
      assert.equal(toBox(r), box, "pattern");
      assert.equal(round(ioiVariance(r)), ioiVar, "IOI variance");
      assert.equal(round(evennessIoi(r)), E, "evenness E");
      assert.equal(round(vertexDistance(r), 2), D, "vertex distance D");
    });
  }

  test("mean evenness E = 0.888 across the eight lanes", () => {
    const mean = LANES.reduce((s, [, k, n]) => s + evennessIoi(bjorklund(k, n)), 0) / LANES.length;
    assert.equal(round(mean), 0.888);
  });

  test("total vertex distance D = 7.67 across the eight lanes", () => {
    const total = LANES.reduce((s, [, k, n]) => s + vertexDistance(bjorklund(k, n)), 0);
    assert.equal(round(total, 2), 7.67);
  });

  test("the clustered control scores E = 0.000", () => {
    for (const [, k, n] of LANES) {
      if (k < 2) continue;
      const clustered = rhythm(Array.from({ length: k }, (_, i) => i), n);
      assert.equal(round(evennessIoi(clustered)), 0);
    }
  });

  test("rotation preserves evenness exactly — the precession lever is safe", () => {
    // Exact in theory; the vertex-distance phase sweep costs a few ulps.
    const r = bjorklund(5, 16);
    for (let s = 0; s < 16; s++) {
      assert.equal(evennessIoi(rotate(r, s)), evennessIoi(r));
      assert.ok(Math.abs(vertexDistance(rotate(r, s)) - vertexDistance(r)) < 1e-9, `rotation ${s}`);
    }
  });
});

// ── dubtek.c timeline analysis, captured from its stderr ────────────────────

describe("dubtek.c parity", () => {
  // name, onsets, n, oddity, evenness, offbeatness — verbatim from `./dubtek`.
  const TIMELINES = [
    ["son 3-2", [0, 3, 6, 10, 12], 16, true, 0.993, 1],
    ["son 2-3", [0, 2, 6, 9, 12], 16, true, 0.993, 1],
    ["rumba 3-2", [0, 3, 7, 10, 12], 16, true, 0.989, 2],
    ["euclid", [0, 3, 6, 10, 13], 16, true, 0.996, 2],
    ["bembe", [0, 2, 4, 5, 7, 9, 11], 12, false, 0.994, 3],
  ];

  for (const [name, onsets, n, oddity, even, offbeat] of TIMELINES) {
    test(name, () => {
      const r = rhythm(onsets, n);
      assert.equal(hasRhythmicOddity(r), oddity, "rhythmic oddity");
      assert.equal(round(evenness(r)), even, "chord-sum evenness");
      assert.equal(offbeatness(r).count, offbeat, "off-beatness");
    });
  }

  test("evenness orders the claves Euclid > son > rumba", () => {
    const e = (o) => evenness(rhythm(o, 16));
    assert.ok(e([0, 3, 6, 10, 13]) > e([0, 3, 6, 10, 12]));
    assert.ok(e([0, 3, 6, 10, 12]) > e([0, 3, 7, 10, 12]));
  });

  test("4 of 5 candidate timelines satisfy rhythmic oddity", () => {
    const passes = TIMELINES.filter(([, o, n]) => hasRhythmicOddity(rhythm(o, n)) === true).length;
    assert.equal(passes, 4);
  });

  test("son<->rumba and son<->euclid are both Hamming 2", () => {
    const son = rhythm([0, 3, 6, 10, 12], 16);
    assert.equal(distHamming(son, rhythm([0, 3, 7, 10, 12], 16)), 2);
    assert.equal(distHamming(son, rhythm([0, 3, 6, 10, 13], 16)), 2);
  });

  test("bembe uniquely attains off-beatness 3 (Toussaint 2005)", () => {
    assert.equal(offbeatness(rhythm([0, 2, 4, 5, 7, 9, 11], 12)).count, 3);
    assert.deepEqual(generators(12), [1, 5, 7, 11]);
  });
});

// ── digest conformance ──────────────────────────────────────────────────────

describe("digest/01 representation", () => {
  test("round-trips through every encoding", () => {
    const r = rhythm("x..x..x.");
    assert.deepEqual(r.onsets, [0, 3, 6]);
    assert.deepEqual(toIoi(r), [3, 3, 2]);
    assert.equal(toBox(fromIoi([3, 3, 2])), "x..x..x.");
    assert.equal(toBox(rhythm("10010010")), "x..x..x.");
    assert.ok(sameNecklace(rhythm([0, 3, 6], 8), "x..x..x."));
  });

  test("the cyclic IOI wrap is not off by one", () => {
    assert.equal(fromIoi([3, 3, 2]).n, 8, "IOIs define n");
    assert.equal(toIoi(rhythm("x..x..x.")).reduce((a, b) => a + b, 0), 8, "and sum back to it");
    assert.throws(() => rhythm([0, 3, 6], 8) && rhythm("x..x..x.", 16), /length 8 != n 16/);
    assert.throws(() => fromIoi([3, 0, 5]), /non-positive/);
  });

  test("necklace equality survives rotation; string equality does not", () => {
    const bossa = rhythm([0, 3, 6, 10, 13], 16);
    const euclid = bjorklund(5, 16);
    assert.notEqual(toBox(bossa), toBox(euclid));
    assert.ok(sameNecklace(bossa, euclid));
  });

  test("bracelet equality additionally survives reflection", () => {
    const r = rhythm([0, 3, 6, 10, 11], 16); // soukous — deliberately asymmetric
    assert.equal(sameNecklace(r, reflect(r)), false);
    assert.ok(sameBracelet(r, reflect(r)));
  });

  test("necklaceCount matches enumeration", () => {
    for (const [n, k] of [[8, 3], [12, 5], [16, 4], [16, 5]]) {
      assert.equal(necklaceCount(n, k), enumerateNecklaces(n, k).length, `N(${n},${k})`);
    }
  });

  test("bracelets are never more numerous than necklaces", () => {
    assert.ok(enumerateBracelets(16, 5).length <= enumerateNecklaces(16, 5).length);
  });
});

describe("digest/02 evenness", () => {
  test("the two-gap property is necessary but NOT sufficient", () => {
    const even = bjorklund(6, 16);
    const impostor = fromIoi([3, 3, 3, 3, 2, 2]);
    assert.ok(gapClasses(even).twoGap);
    assert.ok(gapClasses(impostor).twoGap, "impostor also has two gaps");
    assert.equal(isMaximallyEven(even), true);
    assert.equal(isMaximallyEven(impostor), false, "so the gap test alone would be wrong");
  });

  test("the regular k-gon maximizes chord-sum evenness", () => {
    for (const s of enumerateNecklaces(16, 4)) {
      assert.ok(evenness(s) <= evenness(bjorklund(4, 16)) + 1e-12, s);
    }
  });

  test("balance is not evenness", () => {
    // Two 3-gons a tritone apart: perfectly balanced, plainly not maximally even.
    const balanced = rhythm([0, 1, 4, 5, 8, 9], 12);
    assert.ok(balance(balanced).magnitude < 1e-9, "perfectly balanced");
    assert.equal(isMaximallyEven(balanced), false, "but not maximally even");
    assert.ok(balance(bjorklund(6, 12)).magnitude < 1e-9, "the hexagon is both");
  });

  test("evenly-dividing rhythms score exactly 1", () => {
    for (const [k, n] of [[4, 16], [2, 16], [3, 12]]) {
      assert.equal(round(evennessIoi(bjorklund(k, n))), 1);
      assert.equal(round(vertexDistance(bjorklund(k, n)), 2), 0);
    }
  });

  test("Euclidean rhythms are generated by a single interval", () => {
    assert.notEqual(generatedBy(bjorklund(5, 16)), null);
  });
});

describe("digest/03 oddity, depth, interval content", () => {
  test("rhythmic oddity is undefined, not true, for odd n", () => {
    assert.equal(hasRhythmicOddity(rhythm([0, 2, 4], 7)), null);
  });

  test("the interval vector is not the IOI sequence", () => {
    const son = rhythm([0, 3, 6, 10, 12], 16);
    assert.equal(intervalVector(son).reduce((a, b) => a + b, 0), 10, "C(5,2) pairs");
    assert.equal(toIoi(son).length, 5, "k gaps");
  });

  test("Winograd-deep implies Erdos-deep, not the converse", () => {
    const bembe = rhythm([0, 2, 4, 5, 7, 9, 11], 12);
    assert.ok(isWinogradDeep(bembe), "the bembe/diatonic set is Winograd-deep");
    assert.ok(isErdosDeep(bembe));
    const found = enumerateNecklaces(12, 5).filter((s) => isErdosDeep(s) && !isWinogradDeep(s));
    assert.ok(found.length > 0, "Erdos-deep but not Winograd-deep rhythms exist");
  });

  test("a shelling thins a deep rhythm while it stays deep", () => {
    const bembe = rhythm([0, 2, 4, 5, 7, 9, 11], 12);
    const order = shelling(bembe);
    assert.ok(Array.isArray(order), "bembe is shellable");
    let remaining = bembe.onsets.slice();
    for (const o of order) {
      remaining = remaining.filter((x) => x !== o);
      if (remaining.length >= 2) assert.ok(isErdosDeep(rhythm(remaining, 12)), `still deep at ${remaining}`);
    }
  });

  test("homometric pairs are distinct necklaces with equal interval vectors", () => {
    const pairs = homometricPairs(12, 4);
    assert.ok(pairs.length > 0);
    for (const [a, b] of pairs) {
      assert.ok(sameIntervalVector(a, b));
      assert.equal(sameNecklace(a, b), false);
    }
  });

  test("off-beatness flags its degenerate case at prime n", () => {
    assert.equal(offbeatness(rhythm([0, 3, 6], 8)).degenerate, false);
    assert.equal(offbeatness(rhythm([0, 2, 4], 13)).degenerate, true);
  });

  test("off-beatness is the one measure rotation changes", () => {
    const r = bjorklund(5, 16);
    const counts = new Set(Array.from({ length: 16 }, (_, s) => offbeatness(rotate(r, s)).count));
    assert.ok(counts.size > 1, "off-beatness varies under rotation");
    assert.equal(new Set(Array.from({ length: 16 }, (_, s) => round(evenness(rotate(r, s))))).size, 1);
  });
});

describe("digest/04 syncopation", () => {
  test("the metric hierarchy descends from the downbeat", () => {
    const w = metricWeights(16);
    assert.ok(w[0] > w[8], "downbeat beats the half");
    assert.ok(w[8] > w[4], "half beats the quarter");
    assert.ok(w[4] > w[2] && w[2] > w[1], "and so on down");
  });

  test("an off-beat pattern outscores the downbeat pattern", () => {
    assert.ok(syncopationLhl(rhythm([1, 5, 9, 13], 16)) > syncopationLhl(rhythm([0, 4, 8, 12], 16)));
  });

  test("an isochronous pattern induces a clock with no counterevidence", () => {
    assert.equal(syncopationPovelEssens(rhythm([0, 4, 8, 12], 16)).cScore, 0);
  });
});

describe("digest/05 distance", () => {
  test("son and rumba differ by a single one-pulse swap", () => {
    assert.equal(distSwap(rhythm([0, 3, 6, 10, 12], 16), rhythm([0, 3, 7, 10, 12], 16)), 1);
  });

  test("Hamming is blind to how far an onset moved; swap is not", () => {
    const base = rhythm([0, 4, 8, 12], 16);
    const near = rhythm([1, 4, 8, 12], 16);
    const far = rhythm([7, 4, 8, 12], 16);
    assert.equal(distHamming(base, near), distHamming(base, far), "Hamming cannot tell them apart");
    assert.ok(distSwap(base, near) < distSwap(base, far), "swap can");
  });

  test("the chronotonic vector holds duration context at every pulse", () => {
    assert.deepEqual(chronotonicVector(rhythm([0, 3, 6, 10, 12], 16)),
      [3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 2, 2, 4, 4, 4, 4]);
  });

  test("cyclic distance finds the aligning rotation", () => {
    const r = bjorklund(5, 16);
    const shifted = rotate(r, 6);
    const { distance, rotation } = dist(r, shifted, { measure: "chronotonic" });
    assert.equal(round(distance), 0, "the aligned distance is zero");
    // The reported rotation is the one applied to b to reach a, so it undoes the shift.
    assert.ok(sameNecklace(rotate(shifted, rotation), r));
    assert.equal(rotation, (16 - 6) % 16);
  });

  test("a morph walks from one timeline to the other, staying a rhythm", () => {
    const son = rhythm([0, 3, 6, 10, 12], 16);
    const rumba = rhythm([0, 3, 7, 10, 12], 16);
    const path = morphPath(son, rumba, { align: false });
    assert.ok(path.length >= 2);
    for (const step of path) assert.equal(step.k, son.k, "no onset is ever lost");
    assert.ok(sameNecklace(path[path.length - 1], rumba));
  });
});

describe("digest/06 complements and canons", () => {
  test("a rhythm and its complement interlock exactly", () => {
    const r = bjorklund(5, 16);
    assert.ok(interlocks(r, complement(r)));
    assert.equal(r.k + complement(r).k, 16);
  });

  test("hexachordal theorem: at k = n/2 a rhythm and its complement share an interval vector", () => {
    for (const s of enumerateNecklaces(12, 6).slice(0, 25)) {
      assert.ok(sameIntervalVector(s, complement(s)), s);
    }
  });

  test("tiling partners cover the cycle exactly once", () => {
    const r = rhythm([0, 1], 8);
    const partners = tilingPartners(r);
    assert.ok(partners.length > 0);
    for (const t of partners) assert.ok(isTilingCanon(r, t));
  });

  test("periodicity is detected", () => {
    assert.equal(isPeriodic(bjorklund(4, 16)), true);
    assert.equal(isPeriodic(rhythm([0, 3, 6, 10, 12], 16)), false);
  });
});

describe("analyze()", () => {
  test("reports the full profile for the son clave", () => {
    const a = analyze(rhythm([0, 3, 6, 10, 12], 16));
    assert.equal(a.n, 16);
    assert.equal(a.k, 5);
    assert.equal(a.euclidean, false);
    assert.equal(a.rhythmicOddity, true);
    assert.equal(round(a.evenness), 0.993);
    assert.equal(a.offbeatness.count, 1);
  });
});
