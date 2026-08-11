import test from "node:test";
import assert from "node:assert/strict";
import { color, paste, setBuffer } from "../public/aesthetic.computer/lib/graph.mjs";

// A sprite atlas: one 2×2 red patch at (1,1), the rest green. Pasting a crop of
// the red patch must never bring any green along with it.
const SHEET = 8;
function sheet() {
  const pixels = new Uint8ClampedArray(SHEET * SHEET * 4);
  for (let index = 0; index < SHEET * SHEET; index += 1) {
    pixels[index * 4 + 1] = 200;
    pixels[index * 4 + 3] = 255;
  }
  for (const [x, y] of [[1, 1], [2, 1], [1, 2], [2, 2]]) {
    const at = (y * SHEET + x) * 4;
    pixels[at] = 255;
    pixels[at + 1] = 0;
  }
  return { width: SHEET, height: SHEET, pixels };
}

const CROP = Object.freeze({ x: 1, y: 1, w: 2, h: 2 });

function pasteInto(size, transform, x = 0, y = 0) {
  const destination = { width: size, height: size, pixels: new Uint8ClampedArray(size * size * 4) };
  setBuffer(destination);
  color(255, 255, 255, 255);
  paste(sheet(), x, y, transform);
  let red = 0;
  let green = 0;
  for (let index = 0; index < destination.pixels.length; index += 4) {
    if (destination.pixels[index + 3] === 0) continue;
    if (destination.pixels[index] === 255 && destination.pixels[index + 1] === 0) red += 1;
    if (destination.pixels[index + 1] === 200) green += 1;
  }
  return { red, green };
}

// The fast integer-scale path blits from.width × from.height wholesale and used
// to run even when a crop was asked for, pasting the entire atlas. It was
// guarded by a bounds check against the *sheet* size, so it only misfired when
// the whole sheet happened to fit — which is why cropped sprites looked right
// near a canvas edge and dumped their atlas in the middle.
test("a cropped paste never leaks the rest of the sheet", () => {
  for (const scale of [1, 2, 3, 8]) {
    // Roomy enough that the whole 8×8 sheet would fit if the crop were ignored.
    const { red, green } = pasteInto(64, { scale, crop: CROP }, 10, 10);
    assert.equal(green, 0, `scale ${scale} pasted uncropped sheet pixels`);
    assert.equal(red, 4 * scale * scale, `scale ${scale} pasted the crop at size`);
  }
});

test("the leak does not depend on where the paste lands", () => {
  // Near the origin the sheet fits; near the far edge it does not. Both must
  // give the same cropped result.
  for (const [x, y] of [[0, 0], [10, 10], [60, 60]]) {
    const { green } = pasteInto(64, { scale: 1, crop: CROP }, x, y);
    assert.equal(green, 0, `paste at ${x},${y} leaked`);
  }
});

test("uncropped pastes still take their fast path unchanged", () => {
  const { red, green } = pasteInto(64, { scale: 2 }, 4, 4);
  assert.equal(red, 4 * 4, "the red patch is scaled 2×");
  assert.equal(green, (SHEET * SHEET - 4) * 4, "and the whole sheet comes with it");
});
