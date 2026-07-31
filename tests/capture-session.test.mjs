import test from "node:test";
import assert from "node:assert/strict";
import {
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
