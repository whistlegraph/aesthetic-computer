// ?heat tiling (disk.mjs): what changed goes out as patches, nothing changed
// sends nothing, most of the screen sends it whole. Run: node tests/disk-heat.test.mjs
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
const src = readFileSync(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
const a = src.indexOf("const heat = {"), b = src.indexOf("function heatRows() {");
const { heat, heatChangedBoxes, heatDirtyBox } = new Function(src.slice(a, b) + "\nreturn { heat, heatChangedBoxes, heatDirtyBox };")();
const w = 100, h = 60;
const screen = { width: w, height: h, pixels: new Uint8ClampedArray(w * h * 4) };
const px = new Uint32Array(screen.pixels.buffer);
assert.equal(heatChangedBoxes(screen), undefined, "first frame goes out whole");
assert.deepEqual(heatChangedBoxes(screen), [], "an unchanged frame sends nothing");
px[5 * w + 5] = 0xff0000ff;          // top-left tile
px[50 * w + 90] = 0xff00ff00;        // a tile near the bottom right
const boxes = heatChangedBoxes(screen);
assert.deepEqual(boxes, [{ x: 0, y: 0, w: 16, h: 16 }, { x: 80, y: 48, w: 16, h: 12 }], "two separate patches, the bottom one clipped to the screen");
assert.deepEqual(heatChangedBoxes(screen), [], "after sending, the same frame is quiet");
for (let y = 20; y < 40; y++) px[y * w + 33] = 0xffffffff;  // a vertical stroke across two bands
const tall = heatChangedBoxes(screen);
assert.deepEqual(tall, [{ x: 32, y: 16, w: 16, h: 32 }], "stacked tiles merge into one patch");
const db = heatDirtyBox(tall);
assert.deepEqual(db.croppedBox(), { x: 32, y: 16, w: 16, h: 32 });
const packed = new Uint32Array(db.crop(screen).buffer);
assert.equal(packed.length, 16 * 32);
assert.equal(packed[(20 - 16) * 16 + (33 - 32)], 0xffffffff, "packed pixels line up with their patch");
px.fill(0x12345678);
assert.equal(heatChangedBoxes(screen), undefined, "most of the screen changing sends it whole");
assert.deepEqual(heatChangedBoxes(screen), [], "and then it is quiet again");
// The edge tile is clipped to the screen.
heatChangedBoxes(screen);
px[59 * w + 99] = 1;
assert.deepEqual(heatChangedBoxes(screen), [{ x: 96, y: 48, w: 4, h: 12 }], "the edge tile is clipped to the screen");
console.log("disk heat tiling test passed");
