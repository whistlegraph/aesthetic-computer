import assert from "node:assert/strict";
import test from "node:test";

import { captionPhrases, isHighlightableCaptionToken } from "../lib/captions.mjs";

test("only spoken glyph tokens receive karaoke highlight layers", () => {
  assert.equal(isHighlightableCaptionToken("nœud"), true);
  assert.equal(isHighlightableCaptionToken("3D"), true);
  assert.equal(isHighlightableCaptionToken(":"), false);
  assert.equal(isHighlightableCaptionToken("—"), false);
});

test("caption phrases never overlap and keep punctuation with spoken text", () => {
  const phrases = captionPhrases([
    {
      offsetSec: 2,
      words: [
        { text: "Hello", fromMs: 0, toMs: 410 },
        { text: "world.", fromMs: 390, toMs: 820 },
        { text: "—", fromMs: 820, toMs: 900 },
        { text: "Next", fromMs: 890, toMs: 1210 },
        { text: "step", fromMs: 1210, toMs: 1510 },
      ],
    },
  ], { maxWords: 2 });

  assert.deepEqual(phrases.map((phrase) => phrase.text), ["Hello world. —", "Next step"]);
  for (let index = 0; index + 1 < phrases.length; index += 1) {
    assert.ok(phrases[index].to <= phrases[index + 1].from);
  }
});

test("burned and VTT callers receive one shared deterministic timeline", () => {
  const beats = [{
    offsetSec: 0.5,
    words: [
      { text: "One", fromMs: 0, toMs: 200 },
      { text: "timeline", fromMs: 200, toMs: 500 },
      { text: "only.", fromMs: 500, toMs: 800 },
    ],
  }];
  assert.deepEqual(captionPhrases(beats), captionPhrases(beats));
});
