import test from "node:test";
import assert from "node:assert/strict";
import { episode } from "../marketing/klokkentales/episodes/summer-so-far-2026.mjs";

test("Klokkentales episode has a two-person, disclosed cast", () => {
  assert.equal(episode.slug, "summer-so-far-2026");
  assert.ok(episode.syntheticVoiceDisclosure.includes("synthetic"));
  assert.ok(episode.lines.length >= 20);
  assert.deepEqual(new Set(episode.lines.map((line) => line.speaker)), new Set(["jeffrey", "prutti"]));
  assert.ok(episode.lines.every((line) => line.text.length > 0));
});

test("Klokkentales script excludes raw contact details", () => {
  const text = episode.lines.map((line) => line.text).join(" ");
  assert.equal(/\b\d{8}\b/.test(text), false);
  assert.equal(/\b[A-ZÆØÅ][a-zæøå]+\s+\d+\s+(?:st|tv|th)\b/i.test(text), false);
});

