// Render every recovered No Paint brush to a PNG contact sheet.
//
//   node toolchain/nopaint/contact-sheet.mjs [outDir]
//
// The proposal contracts are pure — `render(api, score, tick)` only ever calls
// ink()/paste() — so they can be drawn straight into a buffer with graph.mjs,
// no browser and no runtime. This is the check unit tests cannot make: whether
// a recovered brush actually looks like the thing it was recovered from.
//
// Each brush gets a row of frames across time on a mid grey ground, so both
// translucent and opaque work is visible.

import { mkdir, writeFile } from "node:fs/promises";
import sharp from "sharp";

const AC = new URL("../../system/public/aesthetic.computer/", import.meta.url);
const load = (path) => import(new URL(path, AC).href);

const graph = await load("lib/graph.mjs");
const { makeProposal, seededRandom } = await load("lib/nopaint-proposals.mjs");

const CELL = 192;
const TICKS = [0, 30, 120, 600, 1800];
const GROUND = [96, 96, 104, 255];

// The subset of the disk API a proposal contract is allowed to touch.
function apiFor(assets) {
  return {
    nopaintAssets: assets,
    paste: (...args) => graph.paste(...args),
    ink: (...color) => {
      graph.color(...(color.length === 1 ? color[0] : color));
      return {
        box: (...a) => graph.box(...a),
        oval: (...a) => graph.oval(...a),
        line: (...a) => graph.line(...a),
        poly: (...a) => graph.poly(...a),
        shape: (...a) => graph.shape(...a),
      };
    },
  };
}

const fresh = (w, h, fill) => {
  const pixels = new Uint8ClampedArray(w * h * 4);
  for (let index = 0; index < pixels.length; index += 4) pixels.set(fill, index);
  return { width: w, height: h, pixels };
};

// Something for the pixel transforms to chew on: colour wedges plus a grid.
function testPainting(size) {
  const painting = fresh(size, size, [0, 0, 0, 255]);
  for (let y = 0; y < size; y += 1) {
    for (let x = 0; x < size; x += 1) {
      const at = (y * size + x) * 4;
      const band = Math.floor(x / (size / 6));
      const shade = 60 + Math.floor(y / size * 180);
      const wedge = [[shade, 40, 40], [shade, shade, 40], [40, shade, 40],
        [40, shade, shade], [40, 40, shade], [shade, 40, shade]][band % 6];
      const grid = (x % 32 < 2 || y % 32 < 2) ? 220 : 0;
      painting.pixels[at] = Math.min(255, wedge[0] + grid);
      painting.pixels[at + 1] = Math.min(255, wedge[1] + grid);
      painting.pixels[at + 2] = Math.min(255, wedge[2] + grid);
      painting.pixels[at + 3] = 255;
    }
  }
  return painting;
}

async function preload(contract) {
  const assets = new Map();
  for (const path of contract.assets || []) {
    const file = new URL(`..${path}`, new URL("public/", new URL("../", AC)).href);
    try {
      const { data, info } = await sharp(file.pathname)
        .ensureAlpha().raw().toBuffer({ resolveWithObject: true });
      assets.set(path, {
        width: info.width, height: info.height,
        pixels: new Uint8ClampedArray(data),
      });
    } catch (error) {
      console.error(`  ! ${path}: ${error.message.split("\n")[0]}`);
    }
  }
  return assets;
}

const CONTRACTS = [];
for (const [module, names] of Object.entries({
  "lib/nopaint-construct-sprites.mjs": ["bubblesProposal", "walkerProposal", "frameProposal"],
  "lib/nopaint-construct-brushes.mjs": ["gridWormProposal", "darkWindowProposal"],
  "lib/nopaint-construct-caterpillar.mjs": ["caterpillarProposal"],
  "lib/nopaint-construct-softy.mjs": ["softyProposal"],
  "lib/nopaint-construct-wafer.mjs": ["waferProposal"],
  "lib/nopaint-construct-shapes.mjs": ["triangleProposal", "ellipseProposal"],
  "lib/nopaint-construct-vignette.mjs": ["vignetteProposal", "auraProposal"],
  "lib/nopaint-construct-build.mjs": ["buildProposal", "bannerProposal"],
  "lib/nopaint-construct-rainbow.mjs": ["rainbowProposal"],
  "lib/nopaint-construct-breathe.mjs": ["breatheProposal"],
  "disks/stamp.mjs": ["nopaintProposal"],
})) {
  const loaded = await load(module);
  for (const name of names) CONTRACTS.push(loaded[name]);
}

const out = new URL(`${process.argv[2] || "tmp/nopaint-sheets"}/`,
  new URL("../../", import.meta.url));
await mkdir(out, { recursive: true });

const seed = process.env.SEED || "look";
for (const contract of CONTRACTS) {
  const assets = await preload(contract);
  const row = fresh(CELL * TICKS.length, CELL, GROUND);
  const random = seededRandom(`${contract.slug}:${seed}`);
  const base = makeProposal(random, CELL, CELL);
  const score = contract.generate({ random, width: CELL, height: CELL, base });

  for (const [index, tick] of TICKS.entries()) {
    const cell = fresh(CELL, CELL, GROUND);
    if (contract.applyPixels) {
      // A transform rewrites what is already there rather than drawing.
      const painting = testPainting(CELL);
      cell.pixels.set(contract.applyPixels(
        painting.pixels, CELL, CELL, score.brush.parameters));
    } else {
      graph.setBuffer(cell);
      contract.render(apiFor(assets), score, tick);
    }
    for (let y = 0; y < CELL; y += 1) {
      const from = y * CELL * 4;
      row.pixels.set(cell.pixels.subarray(from, from + CELL * 4),
        (y * row.width + index * CELL) * 4);
    }
  }

  const file = new URL(`${contract.slug}.png`, out);
  await sharp(Buffer.from(row.pixels), {
    raw: { width: row.width, height: row.height, channels: 4 },
  }).png().toFile(file.pathname);
  const sheets = contract.assets?.length ? ` (${assets.size}/${contract.assets.length} sheets)` : "";
  console.log(`${contract.slug.padEnd(13)} → ${file.pathname}${sheets}`);
}
console.log(`\nticks: ${TICKS.join(", ")}   seed: ${seed}`);
