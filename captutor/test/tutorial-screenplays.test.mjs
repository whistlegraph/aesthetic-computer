import assert from "node:assert/strict";
import test from "node:test";

import basic from "../screenplays/basic-tutorial.mjs";
import imageGeneration from "../screenplays/image-generation-workflow.mjs";
import imageNode from "../screenplays/image-node-tutorial.mjs";

const LOCALES = ["en", "es", "fr"];

for (const screenplay of [basic, imageNode]) {
  test(`${screenplay.slug} has complete English, Spanish, and French narration`, () => {
    assert.ok(screenplay.beats.length >= 7);
    for (const beat of screenplay.beats) {
      for (const locale of LOCALES) {
        assert.equal(typeof beat.say[locale], "string");
        assert.ok(beat.say[locale].length > 12);
      }
    }
  });
}

test("image-generation-workflow has complete English and Chinese narration", () => {
  assert.equal(imageGeneration.billable, true);
  assert.equal(imageGeneration.effectTheme.color, "#111111");
  assert.equal(imageGeneration.effectTheme.glowColor, "#ffffff");
  assert.equal(imageGeneration.effectTheme.scrollIntoView, false);
  assert.doesNotMatch(JSON.stringify(imageGeneration.effectTheme), /7c3aed|purple/i);
  assert.ok(imageGeneration.beats.length >= 8);
  for (const beat of imageGeneration.beats) {
    for (const locale of ["en", "zh-CN"]) {
      assert.equal(typeof beat.say[locale], "string");
      assert.ok(beat.say[locale].length > 12);
    }
  }
  assert.match(imageGeneration.beats.map((beat) => beat.say.en).join(" "), /image-to-image generation/i);
  assert.match(imageGeneration.beats.map((beat) => beat.say["zh-CN"]).join(" "), /图生图/);
});
