import { test } from "node:test";
import assert from "node:assert/strict";
import { readFile, mkdtemp, rm, mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { replayPixels, comparePixelFrames, pixelSuite, hashPixels } from "../kidlisp/conformance/pixels.mjs";

const fixture = async name => JSON.parse(await readFile(new URL(`../kidlisp/conformance/pixels/${name}.json`, import.meta.url)));
const hashes = result => result.frames.map(f => hashPixels(f.rgba));

test("normal module lifecycle matches every golden pixel and repeats exactly", async () => {
  const out = await mkdtemp(join(tmpdir(), "kidlisp-pixel-test-"));
  try { assert.equal((await pixelSuite({ out })).passed, true); }
  finally { await rm(out, { recursive: true, force: true }); }
});

test("isolated concurrent renderers and monitoring preserve pixels", async () => {
  const input = await fixture("random");
  const [a, b, monitored] = await Promise.all([replayPixels(input), replayPixels(input), replayPixels({ ...input, monitor: true })]);
  assert.deepEqual(hashes(a), hashes(b));
  assert.deepEqual(hashes(a), hashes(monitored));
  assert.ok(new Set(hashes(a)).size > 1, "animation must actually change");
});

test("seed, clock, pointer input and drawing changes are visible in the pixels", async () => {
  const random = await fixture("random");
  assert.notDeepEqual(hashes(await replayPixels(random)), hashes(await replayPixels({ ...random, seed: random.seed + 1 })));
  const input = await fixture("clock-input");
  const base = hashes(await replayPixels(input));
  assert.notDeepEqual(base, hashes(await replayPixels({ ...input, epochMs: input.epochMs + 10 })));
  assert.notDeepEqual(base, hashes(await replayPixels({ ...input, events: [] })));
  const primitives = await fixture("primitives");
  const original = await replayPixels(primitives);
  const changed = await replayPixels({ ...primitives, source: primitives.source.replace("(point 88 56)", "(point 87 56)") });
  const diff = comparePixelFrames(original.frames[0].rgba, changed.frames[0].rgba, primitives.width, primitives.height);
  assert.equal(diff.equal, false);
  assert.equal(diff.changedPixels, 2);
});

test("exact comparison catches alpha-only changes and equal-histogram pixel swaps", () => {
  const a = Uint8Array.from([255, 0, 0, 255, 0, 0, 255, 255]);
  const b = Uint8Array.from([0, 0, 255, 255, 255, 0, 0, 255]);
  assert.equal(comparePixelFrames(a, b, 2, 1).changedPixels, 2);
  b.set(a); b[7]--;
  assert.deepEqual(comparePixelFrames(a, b, 2, 1), { equal: false, changedPixels: 1, changedChannels: 1, first: { x: 1, y: 0, before: [0, 0, 255, 255], after: [0, 0, 255, 254] } });
  assert.throws(() => comparePixelFrames(a, b.subarray(0, 4), 2, 1), /viewport/);
});

test("unsupported capabilities, render errors and resource exhaustion cannot pass as blank frames", async () => {
  for (const source of ['(fetch "https://example.com")', '(write "hi")', '(ink rainbow)', '(ink "fade:red-blue")', '($cow)', '(clock "cdefg")', '(ink "unknown-color")', '(line)', '(circle 1 1 999999999)']) {
    await assert.rejects(replayPixels({ source, frames: 1 }));
  }
  await assert.rejects(replayPixels({ source: '(repeat 100 (repeat 100 (box 0 0 1 1)))', frames: 1, maxSteps: 50 }), e => e.code === "STEP_BUDGET");
  assert.throws(() => replayPixels({ source: '(wipe 0)', width: 513 }), /viewport/);
  assert.throws(() => replayPixels({ source: '(wipe 0)', width: 512, height: 512, frames: 240 }), /budget/);
  await assert.rejects(replayPixels({ source: '(wipe 0)' }, { timeoutMs: 1 }), e => e.code === "PIXEL_TIMEOUT");
});

test("missing and tampered golden files fail, and check never updates them", async () => {
  const dir = await mkdtemp(join(tmpdir(), "kidlisp-pixel-negative-"));
  const fixtures = join(dir, "fixtures"), golden = join(dir, "golden"), out = join(dir, "out");
  await mkdir(fixtures);
  await writeFile(join(fixtures, "tiny.json"), JSON.stringify({ source: '(wipe 0) (ink 255) (point 0 0)', width: 2, height: 2, frames: 1 }));
  try {
    await assert.rejects(pixelSuite({ fixtures, golden, out }), /ENOENT/);
    await pixelSuite({ command: "record", fixtures, golden, out });
    const manifestPath = join(golden, "manifest.json");
    const original = await readFile(manifestPath, "utf8");
    const manifest = JSON.parse(original);
    manifest.fixtures["tiny.json"].frames[0].sha256 = "0".repeat(64);
    await writeFile(manifestPath, JSON.stringify(manifest));
    await assert.rejects(pixelSuite({ fixtures, golden, out }), /hash mismatch/);
    assert.equal(JSON.parse(await readFile(manifestPath, "utf8")).fixtures["tiny.json"].frames[0].sha256, "0".repeat(64));
    const { default: sharp } = await import("sharp");
    const file = join(golden, "tiny-000.png");
    const bytes = await sharp(file).ensureAlpha().raw().toBuffer();
    bytes[0]--;
    await sharp(bytes, { raw: { width: 2, height: 2, channels: 4 } }).png().toFile(join(golden, "changed.png"));
    const changed = await readFile(join(golden, "changed.png"));
    await writeFile(file, changed);
    manifest.fixtures["tiny.json"].frames[0].sha256 = hashPixels(bytes);
    await writeFile(manifestPath, JSON.stringify(manifest));
    const failed = await pixelSuite({ fixtures, golden, out });
    assert.equal(failed.passed, false);
    assert.equal(failed.fixtures["tiny.json"][0].baseline.changedPixels, 1);
    assert.ok((await readFile(join(out, "diff-tiny-000.png"))).length > 0);
  } finally { await rm(dir, { recursive: true, force: true }); }
});

test("first-color inspection does not execute operations or consume seeded random state", () => {
  for (const source of ['(once (wipe black))', '(if yes (box))', '(def n 3)', '(repeat 3 (line))', '(box)']) {
    const lisp = new KidLisp({ execution: new KidLispExecution({ seed: 7 }) });
    lisp.ast = lisp.parse(source);
    const seed = lisp.randomState;
    const definitions = { ...lisp.globalDef };
    lisp.detectFirstLineColor();
    assert.equal(lisp.firstLineColor, undefined, source);
    assert.equal(lisp.randomState, seed, source);
    assert.equal(lisp.onceExecuted.size, 0, source);
    assert.deepEqual(lisp.globalDef, definitions, source);
  }
  for (const color of ["red", "rainbow", "c1", "p1"]) {
    const lisp = new KidLisp(); lisp.ast = [[color]]; lisp.detectFirstLineColor();
    assert.equal(lisp.firstLineColor, color);
  }
});
