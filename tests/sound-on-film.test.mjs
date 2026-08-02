import test from "node:test";
import assert from "node:assert/strict";
import {
  attachSoundtrackToFrames,
  frameIndexForSoundtrackProgress,
  normalizeTapeAudio,
} from "../system/public/aesthetic.computer/lib/sound-on-film.mjs";

test("quiet tape audio is raised toward the loudness target", () => {
  const left = new Float32Array([0.02, -0.02, 0.02, -0.02]);
  const right = new Float32Array(left);
  const result = normalizeTapeAudio([left, right]);
  assert.equal(result.gain, 6);
  assert.ok(Math.abs(left[0] - 0.12) < 0.000001);
});

test("tape normalization respects its peak ceiling", () => {
  const channel = new Float32Array([0.1, -1, 0.1, -0.1]);
  const result = normalizeTapeAudio([channel]);
  const ceiling = 10 ** (-1 / 20);
  assert.ok(result.gain <= ceiling);
  assert.ok(Math.max(...channel.map(Math.abs)) <= ceiling);
});

test("near-silent tape audio is not amplified into noise", () => {
  const channel = new Float32Array([0.0001, -0.0001]);
  const result = normalizeTapeAudio([channel]);
  assert.equal(result.normalized, false);
  assert.equal(channel[0], new Float32Array([0.0001])[0]);
});

test("each irregularly timed frame owns a contiguous audio sample range", () => {
  const frames = [[1000], [1010], [1040], [1100]];
  attachSoundtrackToFrames(frames, 4800);
  assert.deepEqual(frames.map((frame) => frame[3]), [
    { audioStart: 0, audioEnd: 300 },
    { audioStart: 300, audioEnd: 1200 },
    { audioStart: 1200, audioEnd: 3000 },
    { audioStart: 3000, audioEnd: 4800 },
  ]);
});

test("the audio read head selects its attached film frame", () => {
  const frames = [[0], [25], [50], [75], [100]];
  attachSoundtrackToFrames(frames, 1000);
  assert.equal(frameIndexForSoundtrackProgress(frames, 0), 0);
  assert.equal(frameIndexForSoundtrackProgress(frames, 0.3), 1);
  assert.equal(frameIndexForSoundtrackProgress(frames, 0.74), 3);
  assert.equal(frameIndexForSoundtrackProgress(frames, 0.99), 4);
});
