import assert from "node:assert/strict";
import test from "node:test";

import basic from "../screenplays/basic-tutorial.mjs";
import imageGeneration from "../screenplays/image-generation-workflow.mjs";
import imageNode from "../screenplays/image-node-tutorial.mjs";
import editorSettings from "../screenplays/editor-settings-tour.mjs";
import appsQuickstart from "../screenplays/apps-quickstart.mjs";
import makingCartoons from "../screenplays/making-cartoons.mjs";

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

test("editor-settings tour is English-first, non-billable, and evidence-gated", () => {
  assert.equal(editorSettings.billable, false);
  assert.ok(editorSettings.beats.length >= 8);
  assert.ok(editorSettings.acceptance.requiredChecks.includes("selection_mode_visible"));
  assert.ok(editorSettings.acceptance.requiredChecks.includes("settings_returned_to_canvas"));
  assert.match(editorSettings.beats.map((beat) => beat.say).join(" "), /connecting, disconnecting/i);
});

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

test("apps quickstart gives every chapter a distinct compact color identity", () => {
  assert.equal(appsQuickstart.shortTitle, "First app");
  assert.equal(appsQuickstart.acceptance.resolution, undefined);
  assert.equal(new Set(appsQuickstart.chapters.map((chapter) => chapter.color)).size,
    appsQuickstart.chapters.length);
  assert.equal(new Set(appsQuickstart.chapters.map((chapter) => chapter.wallpaperColor)).size,
    appsQuickstart.chapters.length);
  for (const chapter of appsQuickstart.chapters) {
    assert.ok(chapter.shortTitle.length <= 12);
    assert.notEqual(chapter.color, chapter.wallpaperColor);
  }
});

test("making-cartoons owns a three-minute two-character image-to-video lesson", () => {
  assert.equal(makingCartoons.title, "Making Cartoons in Fuser");
  assert.equal(makingCartoons.targetDurationSec, 180);
  assert.equal(makingCartoons.actionCuePolicy, "required");
  assert.equal(makingCartoons.acceptance.minimumDurationSec, 165);
  assert.equal(makingCartoons.acceptance.maximumDurationSec, 195);
  assert.equal(makingCartoons.beats.length, 15);
  const script = makingCartoons.beats.map((beat) => beat.say).join(" ");
  assert.match(script, /moon-headed skateboarder/i);
  assert.match(script, /cloud dog/i);
  assert.match(script, /connect the generated image output/i);
  assert.match(script, /same faces, colors, proportions/i);
  assert.ok(script.split(/\s+/).length >= 320);
  for (const beat of makingCartoons.beats) {
    assert.equal(beat.cues.length, 1);
  }
});
