// Build Swift first, then: node experiments/nopaint-brushes/conformance.mjs
import assert from "node:assert/strict";
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { renderBrush, renderDocument } from "./brush.mjs";

const root = fileURLToPath(new URL(".", import.meta.url));
const runner = process.env.BRUSH_RUNNER || `${root}.build/debug/brush-runner`;
const brushes = Object.fromEntries(["line", "wander", "invert"].map(name =>
  [name, JSON.parse(readFileSync(`${root}fixtures/${name === "line" ? "legacy-line" : name}.brush.json`))]));
const invocation = (name, tick = 60, seed = 42) => ({ brush: brushes[name], seed, tick });
function painting(width = 64, height = 32) {
  return { schema: "ac-painting-prototype", version: 1, width, height,
    base: Array.from({ length: width * height }, () => [24, 20, 40, 255]).flat(), steps: [], cursor: 0 };
}
let cases = 0;
function native(request) {
  const result = spawnSync(runner, [], { input: JSON.stringify(request), encoding: "utf8", maxBuffer: 8_000_000 });
  if (result.error) throw result.error;
  return result;
}
function compare(document, preview) {
  const before = JSON.stringify({ document, preview });
  const js = renderDocument(document, preview);
  const result = native({ document, preview });
  assert.equal(result.status, 0, result.stderr);
  const swift = JSON.parse(result.stdout);
  assert.deepEqual(swift.pixels, [...js]);
  assert.deepEqual(swift.document, document, "Swift writer preserves canonical document");
  assert.deepEqual([...renderDocument(swift.document, preview)], [...js], "Swift output reopens in JS");
  assert.equal(JSON.stringify({ document, preview }), before, "Rendering cannot mutate source");
  cases++;
  return js;
}
function rejects(request) {
  assert.throws(() => renderDocument(request.document, request.preview));
  assert.equal(native(request).status, 1, "Native rejects cleanly without crashing");
  cases++;
}

const doc = painting();
for (const amount of [0, 96, 255]) for (const tick of [0, 30, 60]) {
  const partial = structuredClone(invocation("invert", tick));
  Object.assign(partial.brush.operations[0], { amount, durationTicks: 60 });
  const pixels = compare(doc, partial);
  if (amount === 0 || tick === 0) assert.deepEqual([...pixels], doc.base);
}
const firstWalk = renderDocument(doc, invocation("line", 120, 42));
assert.notDeepEqual([...firstWalk], [...renderDocument(doc, invocation("line", 120, 43))], "Restart seed changes gesture");
assert.deepEqual([...firstWalk], [...renderDocument(doc, invocation("line", 120, 42))], "Same seed reproduces gesture");
assert.notDeepEqual([...firstWalk], [...renderDocument(doc, invocation("line", 180, 42))], "Gesture continues growing over time");
for (const size of [[1, 1], [2, 3], [256, 128]]) compare(painting(...size), invocation("line", 1800, 43));
for (const name of Object.keys(brushes)) for (const tick of [0, 1, 30, 60, 3600])
  for (const seed of [0, 42, 0xffffffff]) compare(doc, invocation(name, tick, seed));

// Preview/reject leaves accepted state alone. Acceptance records exact inputs.
const preview = invocation("line");
const proposed = compare(doc, preview);
assert.deepEqual([...renderDocument(doc)], doc.base);
doc.steps.push(preview); doc.cursor++;
assert.deepEqual([...compare(doc)], [...proposed]);
doc.steps.push(invocation("wander", 17)); doc.cursor++;
const beforeInvert = compare(doc);
doc.steps.push(invocation("invert")); doc.cursor++;
compare(doc);
doc.steps.push(invocation("invert")); doc.cursor++;
assert.deepEqual([...compare(doc)], [...beforeInvert], "Double invert restores pixels");
doc.cursor = 1;
assert.deepEqual([...compare(doc)], [...proposed], "Undo uses retained history");
doc.cursor = 4; compare(doc); // redo
compare(JSON.parse(JSON.stringify(doc))); // file round-trip

// Independent numeric expectations, including unpremultiplied alpha.
const dot = structuredClone(invocation("line", 0));
Object.assign(dot.brush.operations[0], { op: "path", durationTicks: 60, points: [{ x: 0, y: 0 }], color: [255, 0, 0, 128], radius: 0 });
assert.deepEqual([...renderBrush([0, 0, 255, 255], 1, 1, dot)], [128, 0, 127, 255]);
assert.deepEqual([...renderBrush([0, 0, 0, 0], 1, 1, dot)], [255, 0, 0, 128]);
const transparent = painting(1, 1); transparent.base = [0, 0, 0, 0]; compare(transparent, dot);
const diagonal = structuredClone(dot);
Object.assign(diagonal.brush.operations[0], { points: [{ x: 0, y: 0 }, { x: 2, y: 2 }], color: [255, 0, 0, 255] });
diagonal.tick = 60;
const small = painting(3, 3); small.base.fill(0);
const diagonalPixels = compare(small, diagonal);
assert.deepEqual(Array.from({ length: 9 }, (_, i) => diagonalPixels[i * 4 + 3]), [255, 0, 0, 0, 255, 0, 0, 0, 255]);
const progressive = structuredClone(diagonal);
progressive.brush.operations[0].points = [{ x: 0, y: 0 }, { x: 4, y: 0 }];
const strip = painting(5, 1); strip.base.fill(0);
for (const [tick, expected] of [[15, [255, 255, 0, 0, 0]], [30, [255, 255, 255, 0, 0]]]) {
  progressive.tick = tick;
  const pixels = compare(strip, progressive);
  assert.deepEqual(Array.from({ length: 5 }, (_, i) => pixels[i * 4 + 3]), expected,
    "A partial segment grows before the next control point is reached");
}

// Deterministic varied inputs cover octants, clipping, alpha, and drift.
let state = 712;
const rand = n => { state = (Math.imul(state, 1664525) + 1013904223) >>> 0; return state % n; };
for (let i = 0; i < 40; i++) {
  const varied = painting(1 + rand(64), 1 + rand(48));
  varied.base = varied.base.map(() => rand(256));
  const step = structuredClone(invocation("wander", rand(3601), rand(0xffffffff)));
  const op = step.brush.operations[0];
  op.points = Array.from({ length: 1 + rand(16) }, () => ({ x: rand(256), y: rand(256) }));
  op.color = Array.from({ length: 4 }, () => rand(256));
  op.radius = rand(5); op.jitter = rand(17);
  op.drift = { x: rand(17) - 8, y: rand(17) - 8 };
  compare(varied, step);
}
for (const change of [
  r => r.document.version = 2,
  r => r.document.width = 0,
  r => r.document.base[0] = 256,
  r => r.document.cursor = 1,
  r => r.preview.brush.version = 99,
  r => r.preview.seed = -1,
  r => r.preview.tick = 0.5,
  r => r.preview.brush.operations[0].op = "fetch",
  r => r.preview.brush.operations[0].radius = 17,
  r => r.preview.brush.operations[0].steps = 0,
  r => { r.document.steps = [structuredClone(r.preview)]; r.document.steps[0].brush.version = 99; },
]) {
  const request = { document: painting(), preview: structuredClone(invocation("line")) };
  change(request); rejects(request);
}
const expensive = structuredClone(invocation("line"));
expensive.brush.operations[0].op = "path";
expensive.brush.operations[0].durationTicks = 60;
expensive.brush.operations[0].radius = 16;
expensive.brush.operations[0].points = Array.from({ length: 128 }, (_, i) => ({ x: i % 2 ? 255 : 0, y: i % 2 ? 255 : 0 }));
rejects({ document: painting(256, 256), preview: expensive });

// A contact sheet made from both independent implementations, enlarged for QA.
const frames = [invocation("line", 30), invocation("wander", 17), invocation("invert")];
const baseDoc = painting(); baseDoc.steps = [invocation("line")]; baseDoc.cursor = 1;
const rows = [frames.map(p => renderDocument(baseDoc, p)),
  frames.map(p => JSON.parse(native({ document: baseDoc, preview: p }).stdout).pixels)];
const scale = 4, width = 64 * 3 * scale, height = 32 * 2 * scale;
const rgb = Buffer.alloc(width * height * 3);
for (let y = 0; y < height; y++) for (let x = 0; x < width; x++) {
  const pixels = rows[Math.floor(y / (32 * scale))][Math.floor(x / (64 * scale))];
  const src = (Math.floor(y / scale) % 32 * 64 + Math.floor(x / scale) % 64) * 4;
  for (let c = 0; c < 3; c++) rgb[(y * width + x) * 3 + c] = pixels[src + c];
}
mkdirSync(`${root}.build/qa`, { recursive: true });
writeFileSync(`${root}.build/qa/parity.ppm`, Buffer.concat([Buffer.from(`P6\n${width} ${height}\n255\n`), rgb]));
// New brushes consume explicit timed input, independent of their seed/style.
const stroke = JSON.parse(readFileSync(`${root}fixtures/line.brush.json`));
const input = [{x: 2, y: 3, tick: 5}, {x: 20, y: 8, tick: 19}, {x: 40, y: 25, tick: 60}];
for (const radius of [0, 1, 6, 16]) for (const tick of [0, 5, 6, 18, 19, 35, 60, 3600]) {
  const brush = structuredClone(stroke); brush.operations[0].radius = radius;
  const call = {brush, seed: 0, tick, gesture: input};
  const pixels = compare(painting(), call);
  assert.deepEqual([...pixels], [...renderDocument(painting(), {...call, seed: 4294967295})], "Brush seed cannot change recorded motion");
  if (tick < 5) assert.deepEqual([...pixels], painting().base);
}
const recorded = {brush: stroke, seed: 42, tick: 35, gesture: input};
const stored = painting(); stored.steps = [recorded]; stored.cursor = 1;
compare(stored);
const thick = structuredClone(recorded); thick.brush.operations[0].radius = 6;
assert.notDeepEqual([...compare(painting(), recorded)], [...compare(painting(), thick)]);
for (const gesture of [[], [{x: -1,y: 0,tick: 0}], [{x: 0,y: 0,tick: 2},{x: 1,y: 1,tick: 2}], [{x: 0,y: 0,tick: 3601}]])
  rejects({document: painting(), preview: {...recorded, gesture}});
const missingGesture = {...recorded}; delete missingGesture.gesture;
rejects({document: painting(), preview: missingGesture});
// Growing an opaque stroke retains every earlier mark; nothing drifts/restarts.
const opaque = structuredClone(recorded); opaque.brush.operations[0].color[3] = 255;
const early = compare(painting(), {...opaque,tick: 19}), late = compare(painting(), {...opaque,tick: 60});
for (let i = 0; i < early.length; i += 4)
  if (early[i] !== painting().base[i]) assert.deepEqual([...late.slice(i,i+4)], [...early.slice(i,i+4)]);
console.log(`${cases} cross-runtime cases passed; JS/Swift document round-trips passed.`);
console.log(`Visual comparison: ${root}.build/qa/parity.ppm (JS above, Swift below).`);
