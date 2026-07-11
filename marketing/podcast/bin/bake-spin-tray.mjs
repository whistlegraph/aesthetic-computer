#!/usr/bin/env node
// bake-spin-tray.mjs — turn ~/Desktop/pals-spin-<slug>/frame_###.png into the
// menu-bar tray sprite sheets under ac-electron/build/icons/pals-spin.
//
// The tray cycles these frames to spin the pals gem. To keep the icon rock
// steady while it turns, every frame is cropped to ONE shared bounding box
// (the union of all frames' opaque pixels) so nothing drifts or clips at the
// widest rotation angle. Then each scale gets a horizontal strip:
//   sheet-1x.png  (height 22)   sheet-2x.png (44)   sheet-3x.png (66)
// plus manifest.json {frames, frameW:{1,2,3}, height:{1,2,3}}.
//
// Usage: node bin/bake-spin-tray.mjs [slug]   (default nat-amethyst)

import sharp from "../../../ac-electron/node_modules/sharp/lib/index.js";
import { readdirSync, mkdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const slug = process.argv[2] || "nat-amethyst";
const SRC = `${homedir()}/Desktop/pals-spin-${slug}`;
const OUT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "..",
  "ac-electron", "build", "icons", "pals-spin");
const H = { 1: 22, 2: 44, 3: 66 };           // tray heights per scale

const frames = readdirSync(SRC).filter((f) => /^frame_\d+\.png$/.test(f)).sort();
if (!frames.length) { console.error(`no frames in ${SRC}`); process.exit(1); }
console.log(`baking ${frames.length} frames from ${slug} …`);

// 1) union opaque bounding box across every frame
let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity, W = 0, Hpx = 0;
const raws = [];
for (const f of frames) {
  const img = sharp(join(SRC, f)).ensureAlpha();
  const { data, info } = await img.raw().toBuffer({ resolveWithObject: true });
  W = info.width; Hpx = info.height;
  raws.push(data);
  for (let y = 0; y < info.height; y++) {
    for (let x = 0; x < info.width; x++) {
      if (data[(y * info.width + x) * 4 + 3] > 8) {
        if (x < minX) minX = x; if (x > maxX) maxX = x;
        if (y < minY) minY = y; if (y > maxY) maxY = y;
      }
    }
  }
}
const pad = 4;
minX = Math.max(0, minX - pad); minY = Math.max(0, minY - pad);
maxX = Math.min(W - 1, maxX + pad); maxY = Math.min(Hpx - 1, maxY + pad);
const cw = maxX - minX + 1, ch = maxY - minY + 1;
const aspect = cw / ch;
console.log(`  union bbox ${cw}×${ch} (aspect ${aspect.toFixed(2)})`);

mkdirSync(OUT, { recursive: true });
const frameW = {};
for (const scale of [1, 2, 3]) {
  const fh = H[scale];
  const fw = Math.round(fh * aspect);
  frameW[scale] = fw;
  const tiles = [];
  for (let i = 0; i < frames.length; i++) {
    const cropped = await sharp(raws[i], { raw: { width: W, height: Hpx, channels: 4 } })
      .extract({ left: minX, top: minY, width: cw, height: ch })
      .resize(fw, fh, { fit: "fill", kernel: "lanczos3" })
      .png().toBuffer();
    tiles.push({ input: cropped, left: i * fw, top: 0 });
  }
  await sharp({ create: { width: fw * frames.length, height: fh, channels: 4, background: { r: 0, g: 0, b: 0, alpha: 0 } } })
    .composite(tiles).png().toFile(join(OUT, `sheet-${scale}x.png`));
  console.log(`  sheet-${scale}x.png  ${fw * frames.length}×${fh}  (frameW ${fw})`);
}
writeFileSync(join(OUT, "manifest.json"),
  JSON.stringify({ frames: frames.length, frameW, height: H }, null, 2) + "\n");
console.log(`✓ baked → ${OUT}`);
