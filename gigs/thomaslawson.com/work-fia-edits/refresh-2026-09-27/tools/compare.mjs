// compare.mjs <beforeDir> <afterDir> <outDir> <viewport> <cropH> [slugs...] → before|after pairs
import { mkdir } from "node:fs/promises";
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import sharp from "sharp";
const [bdir, adir, out, vp = "desktop", cropH = 1500, ...slugs] = process.argv.slice(2);
await mkdir(out, { recursive: true });
const W = vp === "desktop" ? 700 : 360;
for (const slug of slugs) {
  const tiles = [];
  for (const [dir, label] of [[bdir, "before"], [adir, "after"]]) {
    const f = resolve(dir, `${vp}-${slug}.jpg`);
    if (!existsSync(f)) continue;
    const m = await sharp(f).metadata();
    const h = Math.min(m.height, Math.round(+cropH * (m.width / (vp === "desktop" ? 1200 : 480))));
    const img = await sharp(f).extract({ left: 0, top: 0, width: m.width, height: h }).resize({ width: W }).toBuffer();
    const ih = (await sharp(img).metadata()).height;
    const lab = Buffer.from(`<svg width="${W}" height="26"><rect width="100%" height="100%" fill="${label === "before" ? "#555" : "#9b2f5f"}"/><text x="8" y="18" font-size="15" fill="#fff" font-family="Helvetica">${label} · ${slug}</text></svg>`);
    tiles.push({ img, ih, lab });
  }
  if (tiles.length < 2) continue;
  const H = Math.max(...tiles.map((t) => t.ih)) + 26;
  await sharp({ create: { width: W * 2 + 12, height: H, channels: 3, background: "#999" } })
    .composite(tiles.flatMap((t, i) => [{ input: t.lab, left: i * (W + 12), top: 0 }, { input: t.img, left: i * (W + 12), top: 26 }]))
    .jpeg({ quality: 78 }).toFile(resolve(out, `${vp}-${slug}.jpg`));
  console.log(resolve(out, `${vp}-${slug}.jpg`));
}
