import test from "node:test";
import assert from "node:assert/strict";
import { freshLiveReady, latestLiveReady } from "../live-reload.mjs";

const ready = (bytes, generation) => `AC_NATIVE_LIVE_READY bytes=${bytes} generation=${generation}\n`;
test("reload acknowledgement follows the latest application lifetime", () => {
  const before = ready(900, 37);
  const current = before + "AC_NATIVE_BIOS_READY engine=quickjs-ng\n" + ready(1200, 2);
  assert.deepEqual(freshLiveReady(before, current, 1200), { bytes: 1200, generation: 2 });
});
test("reload acknowledgement requires the uploaded byte count", () => {
  assert.equal(freshLiveReady(ready(900, 2), ready(950, 3), 1200), null);
  assert.deepEqual(freshLiveReady(ready(900, 2), ready(1200, 3), 1200),
    { bytes: 1200, generation: 3 });
});
test("old and missing acknowledgements never confirm an upload", () => {
  const log = ready(1200, 3);
  assert.equal(freshLiveReady(log, log + "a noisy frame\n", 1200), null);
  assert.equal(freshLiveReady(log, "a noisy frame\n", 1200), null);
  assert.equal(latestLiveReady(""), null);
});
test("same-sized updates and reset generations with different bytes are recognized", () => {
  assert.deepEqual(freshLiveReady(ready(1200, 3), ready(1200, 4), 1200),
    { bytes: 1200, generation: 4 });
  assert.deepEqual(freshLiveReady(ready(900, 2), ready(1200, 2), 1200),
    { bytes: 1200, generation: 2 });
});
