import test from "node:test";
import assert from "node:assert/strict";
import { shape, clean, wordTimes, TAGS, WORDLESS } from "./scream.mjs";

test("the same words and take are the same request", () => {
  assert.equal(shape("hello world").id, shape("  hello   world ").id);
  assert.notEqual(shape("hello world").id, shape("hello world", 1).id);
  assert.match(shape("hello world").id, /^[a-f0-9]{64}$/);
});

test("words are sent in capitals, the last vowel held, under a tag", () => {
  const s = shape("try now");
  assert.ok(TAGS.includes(s.tag));
  assert.equal(s.said, "TRY NOOOOW!");
  assert.equal(s.body.text, `${s.tag} TRY NOOOOW!`);
  assert.equal(s.text, "TRY NOW");
  assert.deepEqual(s.body.voice_settings, { stability: 0, similarity_boost: 0.9, style: 1, use_speaker_boost: true, speed: 1 });
  assert.equal(shape("why?").said, "WHY?");
});

test("no words is a take from the chaos bank", () => {
  const s = shape("", 3);
  assert.equal(s.tag, WORDLESS[3][0]);
  assert.equal(s.text, WORDLESS[3][1]);
});

test("a caller cannot bring their own tags, or a long line", () => {
  assert.equal(clean("[whispers] hi"), "whispers hi");
  assert.equal(clean("a".repeat(500)).length, 120);
});

test("word times skip the tag and show the caller's words", () => {
  const sent = "[screaming] TRY NOOOOW!";
  const alignment = {
    characters: [...sent],
    character_start_times_seconds: [...sent].map((_, i) => i / 10),
    character_end_times_seconds: [...sent].map((_, i) => (i + 1) / 10),
  };
  assert.deepEqual(wordTimes(alignment, "TRY NOW"), [
    { w: "TRY", s: 1.2, e: 1.5 },
    { w: "NOW", s: 1.6, e: 2.3 },
  ]);
  const bare = { characters: [..."AH AH"], character_start_times_seconds: [0, 1, 2, 3, 4], character_end_times_seconds: [1, 2, 3, 4, 5] };
  assert.deepEqual(wordTimes(bare, "something else entirely").map((w) => w.w), ["AH", "AH"]);
});
