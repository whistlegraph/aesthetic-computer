import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import { darkWindowProposal, gridWormProposal } from "../public/aesthetic.computer/lib/nopaint-construct-brushes.mjs";

const base = Object.freeze({ kind: "placeholder", color: Object.freeze([1, 2, 3, 128]), x: 8, y: 9, w: 40, h: 50, drift: 4, thickness: 2, points: Object.freeze([]), phase: 0 });
const generate = (contract, seed) => contract.generate({ random: seededRandom(seed), width: 640, height: 480, base });

test("Grid Worm is deterministic and retains Construct's quantized grid", () => {
  const score = generate(gridWormProposal, "worm");
  assert.deepEqual(score, generate(gridWormProposal, "worm"));
  assert.ok([32, 64, 128, 256].includes(score.gridSize));
  assert.equal(score.colors.length, 3);
  assert.deepEqual(score.colors.map((color) => color[3]), [153, 204, 153]);
  for (let index = 1; index < score.cells.length; index += 1) {
    const dx = Math.abs(score.cells[index].column - score.cells[index - 1].column);
    const dy = Math.abs(score.cells[index].row - score.cells[index - 1].row);
    assert.equal((dx > 0) !== (dy > 0), true, "each event-sheet step changes exactly one grid axis");
  }
});

test("Dark Window preserves its two-window, four-note rotate/drift state", () => {
  const score = generate(darkWindowProposal, "window");
  assert.deepEqual(score, generate(darkWindowProposal, "window"));
  assert.equal(score.windows.length, 2);
  assert.ok(score.note >= 0 && score.note < 4);
  assert.equal(score.brush.parameters.noteLabel, `Dark Window - Note ${score.note + 1}`);
  assert.equal(score.brush.parameters.rotateStep, 8);
  assert.equal(score.brush.parameters.drift, 0.1);
});
