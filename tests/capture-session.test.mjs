import test from "node:test";
import assert from "node:assert/strict";
import {
  chooseCompactVideoMime,
  getCameraCaptureSize,
  measureCaptureAVSync,
  shouldResumeCaptureRecorder,
} from "../system/public/aesthetic.computer/lib/capture-session.mjs";

test("Back to cap starts a new recorder rather than resuming the prior take", () => {
  assert.equal(shouldResumeCaptureRecorder("paused", true), false);
  assert.equal(shouldResumeCaptureRecorder("paused", false), true);
  assert.equal(shouldResumeCaptureRecorder("inactive", true), false);
});

test("capture A/V alignment reports signed offset and tolerance", () => {
  assert.equal(measureCaptureAVSync(12, 28, 40).offsetMs, 16);
  assert.equal(measureCaptureAVSync(12, 28, 40).aligned, true);
  assert.equal(measureCaptureAVSync(10, 120, 40).aligned, false);
  assert.equal(measureCaptureAVSync(undefined, 20), null);
});

test("compact camera capture requires an MP4 MediaRecorder", () => {
  const supported = new Set(["video/mp4", "video/webm"]);
  assert.equal(
    chooseCompactVideoMime((mime) => supported.has(mime)),
    "video/mp4",
  );
  assert.equal(
    chooseCompactVideoMime((mime) => mime === "video/webm"),
    null,
  );
  assert.equal(chooseCompactVideoMime(undefined), null);
});

test("compact camera capture prefers explicit H264 and AAC", () => {
  assert.equal(
    chooseCompactVideoMime(() => true),
    "video/mp4;codecs=avc1.42E01E,mp4a.40.2",
  );
});

test("recording camera constraints request a useful native resolution", () => {
  assert.deepEqual(getCameraCaptureSize(395, 203, 3), {
    width: 1280,
    height: 658,
  });
  assert.deepEqual(getCameraCaptureSize(1920, 1080, 3), {
    width: 1920,
    height: 1080,
  });
});
