import test from "node:test";
import assert from "node:assert/strict";
import {
  attachSoundtrackToFrames,
  frameIndexForSoundtrackProgress,
} from "../system/public/aesthetic.computer/lib/sound-on-film.mjs";

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
