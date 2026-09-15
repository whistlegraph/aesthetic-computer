import test from "node:test";
import assert from "node:assert/strict";
import { createNoPaintRecording } from "../public/aesthetic.computer/lib/nopaint-recording.mjs";
import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer } from
  "../public/aesthetic.computer/lib/nopaint-pieces.mjs";
import { paste, setBuffer } from "../public/aesthetic.computer/lib/graph.mjs";

test("No Paint exports standard AC snapshots with every accepted layer", () => {
  const width = 4, height = 3;
  const base = new Uint8ClampedArray(width * height * 4).fill(255);
  let piece = createNoPaintPiece({ seed: "recording", width, height, pixels: base });
  const expected = [new Uint8ClampedArray(base)];
  for (let n = 1; n <= 3; n++) {
    const pixels = new Uint8ClampedArray(base.length);
    pixels.set([40 * n, 25, 175, 128], ((n % height) * width + n) * 4);
    const composite = { width, height, pixels: new Uint8ClampedArray(expected.at(-1)) };
    setBuffer(composite);
    paste({ width, height, pixels });
    const layer = createNoPaintProposalLayer({
      piece, proposal: { kind: "line" }, proposalNumber: n, proposalFrame: 120, pixels,
    });
    piece = appendNoPaintLayer(piece, layer, composite.pixels);
    expected.push(new Uint8ClampedArray(composite.pixels));
  }
  // A full-painting transform replaces the prior composite.
  const replacement = new Uint8ClampedArray(base.length).fill(155);
  piece = appendNoPaintLayer(piece, createNoPaintProposalLayer({
    piece, proposal: { kind: "flip" }, proposalNumber: 4, proposalFrame: 1,
    pixels: replacement, pixelMode: "composite",
  }), replacement);
  expected.push(replacement);
  const original = structuredClone(piece);
  const screen = { width: 1, height: 1, pixels: new Uint8ClampedArray(4) };
  let target;
  const record = createNoPaintRecording({
    painting: (w, h) => ({ width: w, height: h, pixels: new Uint8ClampedArray(w * h * 4) }),
    page: (buffer) => { target = buffer; setBuffer(buffer); return { paste }; },
    flatten() {},
    screen,
    num: { timestamp: () => "2026.09.15.15.00.00.000" },
  }, piece);
  assert.equal(record.length, piece.layers.length);
  record.forEach((step, index) => assert.deepEqual(step.painting.pixels, expected[index]));
  assert.equal(new Set(record.map(({ timestamp, label }) => `${timestamp} - ${label}`)).size, record.length,
    "old layers without timestamps still have distinct ZIP filenames");
  assert.deepEqual(piece, original, "export does not alter the accepted painting");
  assert.equal(target, screen, "export restores the display drawing target");
  record[0].painting.pixels.fill(0);
  assert.deepEqual(record.at(-1).painting.pixels, replacement, "recorded frames own their pixels");
});
