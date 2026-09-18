// Headless frame benchmark for a piece that draws with wipe / ink / tri.
// Runs the piece's sim + paint against the real graph.mjs rasterizer in
// node, then again with tri stubbed out, so the frame splits into the
// piece's own math and AC's raster cost.
//
//   node tests/performance/piece-bench.mjs path/to/piece.mjs [width height frames]
//
// Prints one JSON line: triangles per frame, ms of piece math, ms of raster,
// full-frame ms and the fps that implies. Profile a hot piece with
// `node --cpu-prof tests/performance/piece-bench.mjs ...`.
import { pathToFileURL } from "node:url";
import { resolve } from "node:path";

const [, , piecePath, w = "282", h = "256", n = "200"] = process.argv;
if (!piecePath) {
  console.error("usage: piece-bench.mjs <piece.mjs> [width height frames]");
  process.exit(1);
}
const W = +w, H = +h, N = +n;
const graph = await import("../../system/public/aesthetic.computer/lib/graph.mjs");
const piece = await import(pathToFileURL(resolve(piecePath)).href);
const buffer = { pixels: new Uint8ClampedArray(W * H * 4), width: W, height: H };
graph.setBuffer(buffer);

let tris = 0;
const api = (tri) => ({
  screen: { width: W, height: H, pixels: buffer.pixels },
  wipe: (...a) => { graph.color(...graph.findColor(...a)); graph.clear(); },
  ink: (...a) => graph.color(...graph.findColor(...a)),
  tri: (...a) => { tris++; return tri(...a); },
});
const frame = (a) => { piece.sim?.({}); piece.paint(a); };
const time = (a, frames) => {
  const t0 = performance.now();
  for (let i = 0; i < frames; i++) frame(a);
  return (performance.now() - t0) / frames;
};

const stub = api(() => {}), real = api(graph.tri);
for (let i = 0; i < 60; i++) frame(stub); // warm the JIT
tris = 0;
const mathMs = time(stub, N);
const perFrame = Math.round(tris / N);
for (let i = 0; i < 30; i++) frame(real);
const fullMs = time(real, N);
console.log(JSON.stringify({
  piece: piecePath, width: W, height: H, frames: N, trianglesPerFrame: perFrame,
  pieceMathMs: +mathMs.toFixed(2), rasterMs: +(fullMs - mathMs).toFixed(2),
  frameMs: +fullMs.toFixed(2), fps: +(1000 / fullMs).toFixed(1),
}));
