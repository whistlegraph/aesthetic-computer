// Run: node spec/bdf-supplement-spec.mjs
// The 6x10 supplement: generated glyphs fill the gaps, the hand still wins.
import assert from "node:assert/strict";
import { supplementGlyph, hasSupplement } from "../system/backend/bdf-supplement.mjs";

const rows = (glyph) => {
  const grid = Array.from({ length: glyph.resolution[1] }, () => Array(glyph.resolution[0]).fill("."));
  for (const { args: [x, y] } of glyph.commands) grid[y][x] = "#";
  return grid.map((r) => r.join(""));
};

// The letters a reader sent that drew as "?": small capital H, turned e,
// J with stroke. All three come back, on the font's own 6x10 cell.
for (const cp of [0x029c, 0x01dd, 0x0248, 0x1ea1 /* ạ */, 0x04d3 /* ӓ */]) {
  const glyph = supplementGlyph("6x10", cp);
  assert.ok(glyph, `U+${cp.toString(16)} has a glyph`);
  assert.deepEqual(glyph.resolution, [6, 10]);
  assert.deepEqual(glyph.fontMetrics, { ascent: 8, descent: 2 });
  assert.ok(glyph.commands.length > 4, `U+${cp.toString(16)} is drawn, not blank`);
  assert.equal(glyph.supplement, true);
}
assert.deepEqual(rows(supplementGlyph("6x10", 0x01dd)).slice(3, 8), [".###..", "....#.", "#####.", "#...#.", ".###.."], "ǝ is e turned");

// A hand-drawn glyph is never replaced by a generated one.
assert.deepEqual(rows(supplementGlyph("6x10", 0x0268)).slice(1, 8),
  ["..#...", "......", ".##...", "..#...", "#####.", "..#...", ".###.."], "ɨ keeps its hand-drawn stroke");

// Misses stay misses: other fonts, and characters nothing made.
assert.equal(supplementGlyph("unifont", 0x029c), null);
assert.equal(supplementGlyph("6x10", 0x4e00), null);
assert.equal(hasSupplement("6x10", 0x029c), true);
console.log("bdf supplement spec passed: generated 6x10 glyphs, hand-drawn precedence, misses");
