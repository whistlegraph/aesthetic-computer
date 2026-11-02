import test from "node:test";
import assert from "node:assert/strict";
import { describeVoiceMemo, renderVoiceMemo, VOICE_MEMO_NAME, VOICES } from "../lib/voice-memo.mjs";

test("a voice memo refuses nothing, novels and strangers before it speaks", async () => {
  await assert.rejects(renderVoiceMemo("   "), /needs words/);
  await assert.rejects(renderVoiceMemo("x".repeat(1201)), /1201 characters/);
  await assert.rejects(renderVoiceMemo("hi", { voice: "nobody" }), /unknown voice/);
  assert.ok(VOICES.includes("jeffrey"));
  assert.equal(VOICE_MEMO_NAME, "Audio Message.caf");
});

test("the preview line says who speaks, how long, and what", () => {
  const line = describeVoiceMemo({ voice: "jeffrey", seconds: 3.24, bytes: 14826, cached: true, path: "/x/Audio Message.caf", text: "hey fia" });
  assert.equal(line, "voice memo (jeffrey): 3.2s, 14826 bytes, cached — /x/Audio Message.caf\n  says: hey fia");
});
