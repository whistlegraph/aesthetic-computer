import assert from "node:assert/strict";
import test from "node:test";
import { atlasKeys } from "../public/aesthetic.computer/disks/common/emoji-atlas/atlas.mjs";
import { Prompt } from "../public/aesthetic.computer/lib/type.mjs";
import { emojiDraftLayout, emojiMetrics, hasEmoji } from "../public/aesthetic.computer/disks/common/laklok-emoji.mjs";

const font = { blockWidth: 6, blockHeight: 9, getAdvance: () => 6 };
const text = { width: str => str.length * 6 };

test("chat measures complete color emoji while preserving text presentation", () => {
  const metrics = emojiMetrics(text, null, 10);
  for (const emoji of ["🦃", "👩🏽‍🍳", "❤️", "🇩🇰", "1️⃣"]) {
    assert.equal(metrics.width("A" + emoji + "B"), emoji === "🦃" ? 32 : 29);
  }
  for (const plain of ["1", "#", "©", "♥︎"]) assert.equal(hasEmoji(plain), false);
});

test("the shaped draft wraps and moves the caret over whole UTF-16 clusters", () => {
  const prompt = new Prompt(6, 6, "char", 5, 0, font);
  prompt.layoutText = emojiDraftLayout;
  const value = "A🦃👩🏽‍🍳Z";
  prompt.mapTo(value);
  prompt.snapTo(value);
  prompt.crawlBackward();
  assert.equal(prompt.textPos(), value.length - 1);
  prompt.crawlBackward();
  assert.equal(prompt.textPos(), 3);
  assert.equal(prompt.pos().x, 6);
  assert.ok(prompt.pos().y > 6);
  prompt.crawlBackward();
  assert.equal(prompt.textPos(), 1);
  assert.deepEqual(prompt.textToCursorMap[2], prompt.textToCursorMap[1]);
  prompt.crawlForward();
  assert.equal(prompt.textPos(), 3);
  prompt.mapTo("plain");
  assert.equal(prompt.cells, null);
});

test("atlas pixels draw before following bitmap text without a canvas", () => {
  const draws = [];
  const api = {
    ink() { return this; },
    write(value, pos) { draws.push([value, pos.x]); },
    paste(sprite, x) { draws.push([sprite.width, x]); },
  };
  emojiMetrics(text, null, 10).paint(api, "A💙B", 4, 0, "white");
  assert.deepEqual(draws, [["A", 4], [16, 10], ["B", 27]]);
});

test("each official entry paints as one nonempty sprite on its first draw", () => {
  const metrics = emojiMetrics(text, null, 10);
  assert.equal(new Set(atlasKeys).size, 3963 + 9); // Unicode 18 fully-qualified entries and components.
  for (const glyph of atlasKeys) {
    let count = 0;
    metrics.paint({ paste(sprite) {
      count++;
      assert.equal(sprite.height, 16, glyph);
      assert.ok(sprite.pixels.some((channel, i) => i % 4 === 3 && channel > 0), glyph);
    } }, glyph, 0, 0);
    assert.equal(count, 1, glyph);
  }
});

test("presentation selectors preserve text while qualified and unqualified sequences share a cell", () => {
  const metrics = emojiMetrics(text, null, 10);
  for (const plain of ["©", "♥", "1", "#", "🦃︎", "🪼︎"]) assert.equal(hasEmoji(plain), false);
  for (const [variant, canonical] of [["#⃣", "#️⃣"], ["👁‍🗨", "👁️‍🗨️"], ["🦃️", "🦃"]]) {
    assert.equal(metrics.width(variant), metrics.width(canonical));
    assert.ok(hasEmoji(variant));
  }
});

test("atlas artwork scales with nearest-neighbor pixels", () => {
  let sprite;
  const api = { paste(bitmap) { sprite = bitmap; } };
  emojiMetrics(text, null, 10).paint(api, "🫎", 0, 0);
  const base = sprite;
  emojiMetrics(text, null, 33).paint(api, "🫎", 0, 0);
  assert.deepEqual([sprite.width, sprite.height], [32, 32]);
  for (let y = 0; y < 32; y++) {
    for (let x = 0; x < 32; x++) {
      const i = (y * 32 + x) * 4;
      const j = ((y >> 1) * 16 + (x >> 1)) * 4;
      assert.deepEqual(sprite.pixels.subarray(i, i + 4), base.pixels.subarray(j, j + 4));
    }
  }
});

test("turkey has a colored pixel fan, face and wattle without a system font", () => {
  let bitmap;
  const api = { paste(sprite) { bitmap = sprite; } };
  emojiMetrics(text, null, 10).paint(api, "🦃", 0, 0, "white");
  assert.deepEqual([bitmap.width, bitmap.height], [19, 16]);
  assert.deepEqual([...bitmap.pixels.slice((5 * 19 + 14) * 4, (5 * 19 + 14) * 4 + 4)], [115, 219, 233, 255]);
  assert.deepEqual([...bitmap.pixels.slice((7 * 19 + 14) * 4, (7 * 19 + 14) * 4 + 4)], [238, 66, 68, 255]);
});

test("pixel emoji retain color, transparency and advance at two sizes", () => {
  for (const [glyph, width] of [["📻", 18], ["🪼", 16], ["🎵", 15], ["🌿", 16]]) {
    for (const height of [10, 33]) {
      const metrics = emojiMetrics(text, null, height);
      let bitmap;
      metrics.paint({ paste(value) { bitmap = value; } }, glyph, 0, 0, "white");
      const colors = new Set(new Uint32Array(bitmap.pixels.buffer));
      assert.ok(colors.has(0) && colors.size >= 4);
      assert.equal(bitmap.width + 1, metrics.width(glyph));
      assert.equal(bitmap.width, height === 10 ? width : width * 2);
      assert.equal(bitmap.height, height === 10 ? 16 : 32);
    }
  }
});
