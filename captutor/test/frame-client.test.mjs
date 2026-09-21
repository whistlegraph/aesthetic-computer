import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, writeFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { createFrameClient } from "../lib/frame-client.mjs";

test("Captutor imports Frame once, yields during capture, and writes fresh audit pixels", async t => {
  const dir = await mkdtemp(join(tmpdir(), "captutor-frame-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const path = join(dir, "frame.mjs"), out = join(dir, "audit", "frame.jpg");
  await writeFile(path, `
    let calls = 0;
    export async function captureFrame(machine, options) {
      await new Promise(resolve => setTimeout(resolve, 30));
      if (machine !== 'local' || !options.noOCR || !options.noVisual || !options.quietOverlay || !options.memory)
        throw new Error('incorrect capture options');
      return { env: { capture: options.clearOverlays ? 'action' : 'ok', calls: ++calls }, jpg: Buffer.from('fresh') };
    }
  `);
  const capture = createFrameClient(path);
  let ticks = 0;
  const timer = setInterval(() => ticks++, 5);
  try {
    const first = await capture({ screen: true, out });
    assert.equal(first.calls, 1);
    assert.equal(await readFile(out, "utf8"), "fresh");
    assert.ok(ticks > 0, "capture must not block CDP/director messages");
    assert.equal((await capture({ clearOverlays: true })).calls, 2);
  } finally { clearInterval(timer); }
});

test("Captutor rejects denied or empty captures even when an old audit file exists", async t => {
  const dir = await mkdtemp(join(tmpdir(), "captutor-frame-error-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const out = join(dir, "frame.jpg");
  await writeFile(out, "stale");
  for (const status of ["permission_needed", "ok"]) {
    const path = join(dir, `${status}.mjs`);
    await writeFile(path, `export async function captureFrame() { return { env: { capture: '${status}' } }; }`);
    await assert.rejects(createFrameClient(path)({ out }), /capture failed|no pixels/);
    assert.equal(await readFile(out, "utf8"), "stale");
  }
});

test("Captutor preserves custom Frame CLI overrides asynchronously", async t => {
  const dir = await mkdtemp(join(tmpdir(), "captutor-frame-cli-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const path = join(dir, "custom.mjs"), out = join(dir, "frame.jpg");
  await writeFile(path, `
    import {writeFile} from 'node:fs/promises';
    await new Promise(resolve => setTimeout(resolve, 20));
    await writeFile(process.argv[process.argv.indexOf('--out') + 1], 'cli-pixels');
    console.log(JSON.stringify({capture:'ok', args:process.argv.slice(2)}));
  `);
  const result = await createFrameClient(path, { cli: true })({ screen: true, out });
  assert.deepEqual(result.args, ["local", "--no-ocr", "--quiet-overlay", "--json", "--screen", "--out", out]);
  assert.equal(await readFile(out, "utf8"), "cli-pixels");
});
