import test from "node:test";
import assert from "node:assert/strict";
import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer,
  reconcileNoPaintPiece, recoverNoPaintPiece } from "../public/aesthetic.computer/lib/nopaint-pieces.mjs";
import { createNoPaintRecording } from "../public/aesthetic.computer/lib/nopaint-recording.mjs";

const picture = (color, width = 3, height = 2) => ({
  width, height, pixels: new Uint8ClampedArray(width * height * 4).fill(color),
});
const step = (painting, label, timestamp) => ({ painting, label, timestamp });
const exportRecord = (piece) => createNoPaintRecording({
  page() {}, screen: {}, num: { timestamp: () => "now" },
}, piece);

test("AC → No Paint → AC → No Paint retains manual steps and the latest canvas", () => {
  const base = picture(255), red = picture(80), proposed = picture(120);
  const blue = picture(160), green = picture(200);
  const initial = [step(base, "new", "1"), step(red, "line red", "2")];
  let piece = reconcileNoPaintPiece(null, red, initial, "handoff", "2");
  assert.deepEqual(exportRecord(piece).map(({ painting }) => painting), [base, red]);
  const layer = createNoPaintProposalLayer({ piece, proposal: { kind: "line" },
    proposalNumber: 1, proposalFrame: 1, pixels: proposed.pixels, pixelMode: "composite" });
  piece = appendNoPaintLayer(piece, layer, proposed.pixels);
  const original = structuredClone(piece);
  const record = [...exportRecord(piece), step(blue, "line blue", "4"), step(green, "line green", "5")];
  const currentPixels = new Uint8ClampedArray(green.pixels);
  const resumed = reconcileNoPaintPiece(piece, green, record, "another-seed", "6");
  assert.equal(resumed.id, piece.id);
  assert.deepEqual(resumed.layers.slice(0, 3), piece.layers);
  assert.deepEqual(exportRecord(resumed).map(({ painting }) => painting), [base, red, proposed, blue, green]);
  assert.deepEqual(resumed.composite, green);
  assert.deepEqual(green.pixels, currentPixels, "re-entry never overwrites the live canvas");
  assert.deepEqual(piece, original, "the earlier layer history remains intact");
  assert.equal(reconcileNoPaintPiece(resumed, green, record, "ignored", "7"), resumed,
    "returning without edits does not duplicate steps");
  const next = createNoPaintProposalLayer({ piece: resumed, proposal: { kind: "line" },
    proposalNumber: 1, proposalFrame: 1, pixels: base.pixels, pixelMode: "composite" });
  assert(!resumed.layers.some(({ id }) => id === next.id), "restarted proposals have unique layer IDs");
});

test("manual edits without a recording and undo still take priority over the cached composite", () => {
  const base = picture(255), manual = picture(100), undone = picture(160);
  const piece = createNoPaintPiece({ seed: "fallback", ...base });
  const resumed = reconcileNoPaintPiece(piece, manual, [], "ignored", "2");
  assert.deepEqual(resumed.composite, manual);
  const record = [...exportRecord(resumed), step(undone, "line", "3"), { label: "no", timestamp: "4" }];
  const restored = reconcileNoPaintPiece(resumed, base, record, "ignored", "5");
  assert.deepEqual(restored.composite, base);
  assert.deepEqual(exportRecord(restored).at(-1).painting, base);
  assert.equal(reconcileNoPaintPiece(restored, base, record, "ignored", "6"), restored);
  assert.equal(reconcileNoPaintPiece(null, manual, [step(base, "old painting", "1")], "new", "2").layers.length, 1,
    "unrelated AC recordings are not imported");
});

test("a resized AC painting retains its earlier frame dimensions in playback", () => {
  const base = picture(255), resized = picture(100, 5, 4);
  const piece = createNoPaintPiece({ seed: "resize", ...base });
  const resumed = reconcileNoPaintPiece(piece, resized,
    [step(base, "new", "1"), step(resized, "crop", "2")], "ignored", "3");
  const recovered = recoverNoPaintPiece(structuredClone(resumed), 5, 4);
  assert.deepEqual(exportRecord(recovered).map(({ painting }) => painting), [base, resized]);
  assert.equal(recovered.width, 5);
  assert.equal(recovered.height, 4);
});
