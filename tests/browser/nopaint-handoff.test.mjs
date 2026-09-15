// Run with AC_TEST_URL=http://localhost:8892 against ac-static-server.mjs.
import assert from "node:assert/strict";
import JSZip from "jszip";
import sharp from "sharp";
import { ACSession, CONFIG } from "./ac-harness.mjs";
import { mockNoPaintUploads } from "./nopaint-upload-mock.mjs";

const ac = await ACSession.open();
const uploads = await mockNoPaintUploads(ac.page, CONFIG.baseURL);
const fingerprint = (pixels) => {
  let hash = 2166136261;
  const stride = Math.max(1, Math.floor(pixels.length / 4096));
  for (let i = 0; i < pixels.length; i += stride) { hash ^= pixels[i]; hash = Math.imul(hash, 16777619); }
  return (hash >>> 0).toString(16).padStart(8, "0");
};
async function recordedPaintings() {
  return ac.page.evaluate(async () => {
    const store = await import("/aesthetic.computer/lib/store.mjs");
    const record = await store.get("painting:record") || [];
    return record.filter((step) => step.painting).map(({ label, painting }) => {
      let hash = 2166136261;
      const stride = Math.max(1, Math.floor(painting.pixels.length / 4096));
      for (let i = 0; i < painting.pixels.length; i += stride) {
        hash ^= painting.pixels[i]; hash = Math.imul(hash, 16777619);
      }
      return { label, width: painting.width, height: painting.height, fingerprint: (hash >>> 0).toString(16).padStart(8, "0") };
    });
  });
}
async function jump(piece) {
  const previousSeed = (await ac.nopaintState())?.seed;
  await ac.page.evaluate((piece) => window.acSEND({ type: "jump", content: { piece, ahistorical: false, alias: false } }), piece);
  if (piece === "nopaint") {
    await ac.page.waitForFunction((seed) => {
      const state = window.__acNoPaintTest?.();
      return state?.ready && state.seed !== seed;
    }, { timeout: 30000 }, previousSeed);
  } else {
    await ac.page.waitForFunction(() => location.pathname.startsWith("/line"), { timeout: 30000 });
  }
  await ac.wait(700);
}
async function stroke(y) {
  await ac.page.mouse.move(160, y);
  await ac.page.mouse.down();
  await ac.page.mouse.move(580, y + 90, { steps: 14 });
  await ac.page.mouse.up();
  await ac.wait(400);
}
async function click(box) {
  const state = await ac.nopaintState();
  const rect = await ac.page.evaluate(() => {
    const r = [...document.querySelectorAll("canvas")].map((c) => c.getBoundingClientRect())
      .sort((a, b) => b.width * b.height - a.width * a.height)[0];
    return { x: r.x, y: r.y, w: r.width, h: r.height };
  });
  await ac.page.mouse.click(rect.x + (box.x + box.w / 2) * rect.w / state.layout.screenResolution.width,
    rect.y + (box.y + box.h / 2) * rect.h / state.layout.screenResolution.height);
  await ac.wait(200);
}

try {
  await ac.page.setViewport({ width: 760, height: 620 });
  await ac.boot("line:3~red?noauth=1&workerbundle=1");
  await ac.page.mouse.click(380, 300);
  await ac.wait(1200);
  await stroke(190);
  await jump("nopaint");
  const initial = await recordedPaintings();
  assert(initial.length >= 2);
  let state = await ac.nopaintState();
  assert.equal(state.paintingFingerprint, initial.at(-1).fingerprint, "first entry imports the manual canvas");
  assert.equal(state.piece.layerCount, initial.length, "first entry keeps the AC brush steps");
  await ac.measureNopaintDecision("ArrowRight");
  const accepted = await ac.nopaintState();
  await jump("line:4~blue");
  await stroke(290);
  await stroke(380);
  await jump("nopaint");
  const manual = await recordedPaintings();
  assert.notEqual(manual.at(-1).fingerprint, accepted.paintingFingerprint);
  const returned = await ac.nopaintState();
  assert.equal(returned.piece.id, accepted.piece.id, "the same painting continues");
  assert.equal(returned.paintingFingerprint, manual.at(-1).fingerprint, "manual edits survive re-entry");
  assert.equal(returned.piece.layerCount, accepted.piece.layerCount + 2, "both manual strokes join the history");
  await ac.shot("nopaint-handoff/manual-edits-returned");
  await jump("line:4~blue");
  await jump("nopaint");
  state = await ac.nopaintState();
  assert.equal(state.paintingFingerprint, returned.paintingFingerprint);
  assert.equal(state.piece.layerCount, returned.piece.layerCount, "an unchanged visit adds no duplicate step");
  await ac.measureNopaintDecision("ArrowRight");
  const final = await ac.nopaintState();
  await click(final.paintingButton);
  await click((await ac.nopaintState()).controls.done);
  await ac.page.waitForFunction(() => window.__acNoPaintTest?.()?.completion.code === "test", { timeout: 30000 });
  const zip = await JSZip.loadAsync(uploads.files.zip);
  const steps = JSON.parse(await zip.file("painting.json").async("string"));
  const expected = [...manual.map((step) => step.fingerprint), final.paintingFingerprint];
  assert.equal(steps.length, expected.length, "saved playback includes AC and No Paint steps");
  for (let i = 0; i < steps.length; i++) {
    const png = await zip.file(`${steps[i].step}.png`).async("nodebuffer");
    assert.equal(fingerprint(await sharp(png).ensureAlpha().raw().toBuffer()), expected[i], `saved frame ${i}`);
  }
  console.log(JSON.stringify({ result: "passed", steps: steps.length, checks: [
    "manual AC painting imports with its steps", "No Paint → AC brush → No Paint retains both manual strokes",
    "same piece identity", "no duplicate steps on unchanged return", "saved ZIP matches the whole mixed history",
  ] }));
} finally { await ac.close(); }
