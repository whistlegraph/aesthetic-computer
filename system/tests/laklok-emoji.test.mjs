import assert from "node:assert/strict";
import test from "node:test";
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

test("missing canvas fonts reserve the same width before following text", () => {
  const draws = [];
  const api = { ink() { return this; }, write(value, pos) { draws.push([value, pos.x]); } };
  emojiMetrics(text, null, 10).paint(api, "A💙B", 4, 0, "white");
  assert.deepEqual(draws, [["A", 4], ["?", 10], ["B", 27]]);
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
