// P-centers — physical onset is not perceived onset.
//
// Wessel, CMJ 3(2) 1979 p.50: altering a tone's attack moves where the note is
// HEARD to start, so a line whose timbre changes note to note reads as
// rhythmically uneven even when every physical onset is exactly on the grid.

import test from "node:test";
import assert from "node:assert";
import {
  parseMelody,
  perceivedOnsetLagMs,
  pCenterShiftsMs,
  applyPCenterShifts,
} from "../system/public/aesthetic.computer/lib/melody-parser.mjs";
import { WAVE_TIMBRE } from "../system/public/aesthetic.computer/lib/sound/wave-timbre.mjs";

const starts = (notes, base = 500) => {
  const out = [];
  let acc = 0;
  for (const n of notes) {
    out.push(acc);
    acc += (n.duration || 1) * base;
  }
  return out;
};

test("a slow-attack wave lags a fast one", () => {
  // whistle takes ~46 ms to reach level; square is instantaneous.
  assert.ok(WAVE_TIMBRE.whistle.riseMs > 40);
  assert.equal(WAVE_TIMBRE.square.riseMs, 0);
  assert.ok(perceivedOnsetLagMs("whistle") > perceivedOnsetLagMs("square"));
  assert.equal(perceivedOnsetLagMs("square"), 0);
});

test("an unmeasured mode contributes no correction", () => {
  assert.equal(perceivedOnsetLagMs("gm"), 0);
  assert.equal(perceivedOnsetLagMs("nonsense"), 0);
});

test("a single-timbre line is left exactly alone", () => {
  // Every note lags identically, so the mean-zero correction is all zeros.
  // This is the guarantee that the feature cannot disturb existing music.
  const notes = parseMelody("{whistle}cdefg");
  assert.deepEqual(pCenterShiftsMs(notes), [0, 0, 0, 0, 0]);
  const s = starts(notes);
  assert.deepEqual(applyPCenterShifts(s, notes), s);
});

test("a mixed line pulls the slow wave earlier and the fast wave later", () => {
  const notes = parseMelody("{square}c{whistle}d{square}e{whistle}f");
  const shifts = pCenterShiftsMs(notes);
  assert.ok(shifts[0] > 0, "square should sit later than the naive grid");
  assert.ok(shifts[1] < 0, "whistle should be pulled earlier");
  assert.ok(
    Math.abs(shifts[1] - shifts[0]) > 20,
    `expected a >20ms spread, got ${shifts}`,
  );
});

test("corrections are mean-zero, so tempo never drifts", () => {
  const notes = parseMelody("{square}c{whistle}d{sine}e{harp}f{sawtooth}g");
  const shifts = pCenterShiftsMs(notes);
  const sum = shifts.reduce((a, b) => a + b, 0);
  assert.ok(Math.abs(sum) < 1e-9, `shifts should cancel, summed to ${sum}`);
});

test("rests do not vote on the average", () => {
  const withRest = parseMelody("{square}c-{square}d");
  // Every SOUNDING note is a square, so nothing should move even though the
  // rest carries a wave type of its own.
  assert.deepEqual(
    pCenterShiftsMs(withRest).filter((s) => s !== 0),
    [],
  );
});

test("corrected starts stay ordered and never go negative", () => {
  const notes = parseMelody("{whistle}c...{square}d...{whistle}e...{square}f...");
  const corrected = applyPCenterShifts(starts(notes, 50), notes);
  assert.equal(corrected.length, notes.length);
  assert.ok(corrected[0] >= 0, "first start went negative");
  for (let i = 1; i < corrected.length; i += 1) {
    assert.ok(
      corrected[i] >= corrected[i - 1],
      `starts went backwards at ${i}: ${corrected}`,
    );
  }
});

test("the loop length is not the timeline's to change", () => {
  const notes = parseMelody("{square}c{whistle}d{square}e{whistle}f");
  const s = starts(notes);
  const corrected = applyPCenterShifts(s, notes);
  // `_loopMs` in kidlisp comes from the uncorrected accumulator, so the only
  // thing that must hold here is that corrections stay inside the bar.
  const loopMs = s[s.length - 1] + (notes[notes.length - 1].duration || 1) * 500;
  assert.ok(corrected[corrected.length - 1] < loopMs);
});

test("a length mismatch is refused rather than silently mangled", () => {
  const notes = parseMelody("{square}c{whistle}d");
  assert.deepEqual(applyPCenterShifts([0, 500, 1000], notes), [0, 500, 1000]);
});
