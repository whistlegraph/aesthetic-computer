#!/usr/bin/env node
// render.mjs — offline cover-art render for the "slippy fish" oskie single.
//
// Loads system/.../disks/fish.mjs HEADLESSLY (fish.mjs is READ-ONLY — another
// process edits it). Uses the proven strip-export + data-URL + globalThis
// injection trick to reach the offline `stepOverride` hook, then renders
// full-res stills by walking screen.pixels directly.
//
// Modes:
//   node render.mjs sweep         -> small preview PNGs across time/yaw/zoom
//   node render.mjs final T Y Z   -> 3000x3000 JPG at chosen params
//   node render.mjs final T Y Z 6000 -> 6000x6000 supersample -> 3000 (AA)
//
// Working files land in ~/Documents/Shelf/slippy-fish/ (Desktop is auto-cleaned
// mid-render on this machine). The final JPG is COPIED to ~/Desktop only after
// it is fully written.

import { readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import os from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const FISH = resolve(REPO, "system/public/aesthetic.computer/disks/fish.mjs");
const SHELF = resolve(os.homedir(), "Documents/Shelf/slippy-fish");
const PREVIEWS = resolve(SHELF, "previews");
mkdirSync(PREVIEWS, { recursive: true });

const sharp = (await import(resolve(REPO, "ac-electron/node_modules/sharp/lib/index.js"))).default;

// --- load fish.mjs headlessly (read-only) ---------------------------------
let src = readFileSync(FISH, "utf8").replace(/export \{[^}]*\};?/g, "");
await import(
  "data:text/javascript," +
    encodeURIComponent(
      src +
        `
  globalThis.__paint = paint;
  globalThis.__boot  = boot;
  globalThis.__cfg = (o) => {
    if ("step" in o) stepOverride = o.step;
    if ("time" in o) time = o.time;
    if ("yaw"  in o) { yaw = o.yaw; targetYaw = o.yaw; }
    if ("zoom" in o) { zoom = o.zoom; targetZoom = o.zoom; }
    dragging = true; // disable idle drift
  };
`,
    ),
);

function renderStill(W, H, { time, yaw, zoom }) {
  globalThis.__boot();
  globalThis.__cfg({ step: 1, time, yaw, zoom });
  const pixels = new Uint8ClampedArray(W * H * 4);
  globalThis.__paint({ screen: { width: W, height: H, pixels } });
  return pixels;
}

const mode = process.argv[2] || "sweep";

if (mode === "sweep") {
  const W = 360,
    H = 360;
  // Candidate hero framings. time = orbit phase (fish position + heading),
  // yaw = camera spin, zoom = camera distance (3.0 close .. 4.0 wide).
  const cands = [
    { name: "a", time: 1.2, yaw: 0.5, zoom: 3.3 },
    { name: "b", time: 3.1, yaw: -0.4, zoom: 3.2 },
    { name: "c", time: 4.7, yaw: 0.9, zoom: 3.5 },
    { name: "d", time: 6.3, yaw: 1.6, zoom: 3.1 },
    { name: "e", time: 7.9, yaw: -1.1, zoom: 3.4 },
    { name: "f", time: 9.4, yaw: 0.2, zoom: 3.0 },
    { name: "g", time: 10.8, yaw: 2.2, zoom: 3.6 },
    { name: "h", time: 2.0, yaw: -2.0, zoom: 3.25 },
  ];
  for (const c of cands) {
    const px = renderStill(W, H, c);
    const out = resolve(PREVIEWS, `preview-${c.name}.png`);
    await sharp(Buffer.from(px), { raw: { width: W, height: H, channels: 4 } })
      .png()
      .toFile(out);
    console.log(
      `preview ${c.name}: time=${c.time} yaw=${c.yaw} zoom=${c.zoom} -> ${out}`,
    );
  }
  console.log("\nSweep done. Inspect previews in", PREVIEWS);
} else if (mode === "final") {
  const time = parseFloat(process.argv[3]);
  const yaw = parseFloat(process.argv[4]);
  const zoom = parseFloat(process.argv[5]);
  const SS = process.argv[6] ? parseInt(process.argv[6]) : 3000;
  const TARGET = 3000;
  console.log(`Rendering ${SS}x${SS} (step:1) time=${time} yaw=${yaw} zoom=${zoom} ...`);
  const t0 = Date.now();
  const px = renderStill(SS, SS, { time, yaw, zoom });
  console.log(`  raymarch done in ${((Date.now() - t0) / 1000).toFixed(1)}s`);

  const shelfOut = resolve(SHELF, "slippy-fish-render-3000.jpg");
  let pipe = sharp(Buffer.from(px), { raw: { width: SS, height: SS, channels: 4 } });
  if (SS !== TARGET) pipe = pipe.resize(TARGET, TARGET, { kernel: "lanczos3" });
  await pipe.jpeg({ quality: 92 }).toFile(shelfOut);
  console.log(`  wrote ${shelfOut}`);
} else {
  console.error("usage: render.mjs sweep | final T Y Z [SS]");
  process.exit(1);
}
