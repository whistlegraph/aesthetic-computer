#!/usr/bin/env node
// make-qrs.mjs — pre-bake a QR image per curated whistlegraph, for the
// ffmpeg-native playout to overlay. Runs where the repo lives (the qr
// encoder is AC's own), writes P6 PPMs (ffmpeg reads them natively, no
// PNG encoder needed) into <out-dir>/<code>.ppm.
//
//   node make-qrs.mjs <curation-json> <out-dir> [module-px] [quiet-modules]
//
// Each QR points at https://whistlegraph.org/<code> — the same walk-home
// link the browser TV draws in its corner.
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const { qrcode, ErrorCorrectLevel } = await import(
  resolve(HERE, "../../system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs")
);

const [curationPath, outDir, mpxArg, qzArg] = process.argv.slice(2);
if (!curationPath || !outDir) {
  console.error("usage: make-qrs.mjs <curation-json> <out-dir> [module-px] [quiet-modules]");
  process.exit(1);
}
const MPX = parseInt(mpxArg || "6", 10);   // pixels per QR module
const QZ = parseInt(qzArg || "3", 10);     // quiet-zone width in modules
mkdirSync(outDir, { recursive: true });

const curation = JSON.parse(readFileSync(curationPath, "utf8"));
const codes = Object.entries(curation.works).map(([k, w]) => w.asset || k);

for (const code of codes) {
  const cells = qrcode(`https://whistlegraph.org/${code}`, {
    errorCorrectLevel: ErrorCorrectLevel.M,
  }).modules;
  const n = cells.length;
  const dim = (n + QZ * 2) * MPX; // square, quiet zone baked in
  // P6 header + RGB triples. Paper #fffdf6, ink #25204a — the archive's palette.
  const header = `P6\n${dim} ${dim}\n255\n`;
  const buf = Buffer.alloc(header.length + dim * dim * 3);
  buf.write(header, 0, "ascii");
  let o = header.length;
  const paper = [255, 253, 246], ink = [37, 32, 74];
  for (let y = 0; y < dim; y++) {
    const my = Math.floor(y / MPX) - QZ;
    for (let x = 0; x < dim; x++) {
      const mx = Math.floor(x / MPX) - QZ;
      const dark = my >= 0 && my < n && mx >= 0 && mx < n && cells[my][mx];
      const [r, g, b] = dark ? ink : paper;
      buf[o++] = r; buf[o++] = g; buf[o++] = b;
    }
  }
  writeFileSync(resolve(outDir, `${code}.ppm`), buf);
}
console.log(`✓ ${codes.length} QR PPMs → ${outDir} (${MPX}px/module, ${QZ}-module quiet zone)`);
