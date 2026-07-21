import assert from "node:assert/strict";
import test from "node:test";

import basic from "../screenplays/basic-tutorial.mjs";
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
