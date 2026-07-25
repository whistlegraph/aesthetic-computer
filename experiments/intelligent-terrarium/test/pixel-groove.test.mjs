import assert from "node:assert/strict";
import test from "node:test";
import {
  GROOVE_LAYOUT, grooveCoordinates, inspectPixelGroove, printPixelGroove,
  startPixelGroove, stepPixelGroove,
} from "../src/pixel-groove.mjs";

const instruction = Buffer.alloc(24, 0);
instruction[0] = 3;

test("PixelGroove maps every RGB margin pixel exactly once", () => {
  const coordinates = grooveCoordinates();
  assert.equal(coordinates.length, GROOVE_LAYOUT.pixels);
  assert.equal(new Set(coordinates.map(([x, y]) => `${x}:${y}`)).size, GROOVE_LAYOUT.pixels);
  assert.ok(coordinates.every(([x, y]) => x < 16 || y < 16 || x >= 144 || y >= 144));
});

test("PixelGroove advances mutable needle state without weakening protected tracks", () => {
  const sequence = Buffer.concat([instruction, instruction]);
  const groove = startPixelGroove({ id: "groove-test", source: "(raster (shift 1 0))", bytecode: sequence });
  const initial = inspectPixelGroove(groove);
  assert.equal(initial.valid, true);
  assert.equal(initial.pc, 0);
  assert.ok(initial.density.tracks.source.occupiedBytes > 0);
  assert.equal(initial.density.tracks.fringe.occupiedPixels, 0);
  const stepped = stepPixelGroove(groove);
  assert.equal(stepped.record.valid, true);
  assert.equal(stepped.record.pc, 1);
  assert.deepEqual(Buffer.from(stepped.instruction), instruction);

  const tampered = groove.slice();
  tampered[GROOVE_LAYOUT.tracks.source.base * 3] ^= 1;
  const record = inspectPixelGroove(tampered);
  assert.equal(record.valid, false);
  assert.ok(record.errors.includes("protected hash mismatch"));
});

test("PixelGroove prints a pixel-perfect 160x160 PPM substrate", () => {
  const groove = startPixelGroove({ id: "groove-print", source: "(raster (shift 1 0))", bytecode: instruction });
  const field = new Uint8Array(128 * 128 * 3).fill(37);
  const ppm = printPixelGroove(groove, { field });
  const header = Buffer.from("P6\n160 160\n255\n");
  assert.deepEqual(ppm.subarray(0, header.length), header);
  assert.equal(ppm.length, header.length + 160 * 160 * 3);
});

test("PixelGroove protects hardware class and entropy authority in its header", () => {
  const groove = startPixelGroove({
    id: "groove-clock", source: "(raster (shift 1 0))", bytecode: instruction,
    profile: "double", authorityUtcMs: 1_784_914_267_123,
    entropySource: "ac-utc+memory", entropySeed: 0xdecafbad,
  });
  const record = inspectPixelGroove(groove);
  assert.equal(record.valid, true);
  assert.equal(record.hardware.name, "double");
  assert.equal(record.hardware.resolution, 256);
  assert.equal(record.authorityUtcMs, 1_784_914_267_123);
  assert.equal(record.entropySource, "ac-utc+memory");
  assert.equal(record.entropySeed, 0xdecafbad);
  const field = new Uint8Array(256 * 256 * 3).fill(61);
  assert.match(printPixelGroove(groove, { field }).subarray(0, 16).toString(), /^P6\n160 160\n255\n/);
});
