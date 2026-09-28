// sheet.mjs <evidenceDir> <viewport> <cols> <cropH> [slugFilterRegex] → contact sheets (12 per sheet)
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import sharp from "sharp";

const [dir, vp = "desktop", cols = 4, cropH = 1500, filter] = process.argv.slice(2);
const m = JSON.parse(await readFile(resolve(dir, "manifest.json"), "utf8"));
const items = m.pages.filter((p) => p.viewport === vp && (!filter || new RegExp(filter).test(p.slug)));
const W = vp === "desktop" ? 1200 : 480;
const tileW = vp === "desktop" ? 600 : 300;
const scale = tileW / W;
const tileH = Math.round(+cropH * scale);
const per = +cols * 3;
for (let s = 0; s * per < items.length; s++) {
  const chunk = items.slice(s * per, s * per + per);
  const rows = Math.ceil(chunk.length / cols);
  const comps = [];
  for (const [i, p] of chunk.entries()) {
    const img = sharp(resolve(dir, p.file));
    const meta = await img.metadata();
    const buf = await img.extract({ left: 0, top: 0, width: meta.width, height: Math.min(meta.height, +cropH) })
      .resize({ width: tileW }).extend({ bottom: tileH + 24 - Math.round(Math.min(meta.height, +cropH) * scale), background: "#ccc" }).toBuffer();
    const label = Buffer.from(`<svg width="${tileW}" height="22"><rect width="100%" height="100%" fill="#111"/><text x="6" y="16" font-size="14" fill="#fff" font-family="Helvetica">${p.slug}</text></svg>`);
    comps.push({ input: buf, left: (i % cols) * (tileW + 8), top: Math.floor(i / cols) * (tileH + 32) + 22 });
    comps.push({ input: label, left: (i % cols) * (tileW + 8), top: Math.floor(i / cols) * (tileH + 32) });
  }
  const out = resolve(dir, `sheet-${vp}-${s + 1}.jpg`);
  await sharp({ create: { width: cols * (tileW + 8), height: rows * (tileH + 32) + 30, channels: 3, background: "#888" } })
    .composite(comps).jpeg({ quality: 70 }).toFile(out);
  console.log(out);
}
