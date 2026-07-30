import assert from "node:assert/strict";
import test from "node:test";
import { audioType, ownsKey, safeTrackName, userPrefix } from "../../system/netlify/functions/juke-cloud.mjs";

test("track names cannot escape the Juke prefix", () => {
  assert.equal(safeTrackName("../../mix ? 4.wav"), "mix - 4.wav");
  assert.equal(userPrefix("auth0|one"), "auth0|one/jukewizard/");
  assert.equal(ownsKey("auth0|one", "auth0|one/jukewizard/id-mix.wav"), true);
  assert.equal(ownsKey("auth0|one", "auth0|two/jukewizard/id-mix.wav"), false);
  assert.equal(ownsKey("auth0|one", "auth0|one/jukewizard/../private.wav"), false);
});

test("only audio extensions receive content types", () => {
  assert.equal(audioType("demo.MP3"), "audio/mpeg");
  assert.equal(audioType("demo.aiff"), "audio/aiff");
  assert.equal(audioType("demo.json"), null);
});
