// how densely can the picture be packed before the spectrogram smears it?
// time-frequency uncertainty sets a hard ceiling on independent cells per
// second; this finds where the geometry actually lands relative to it.

import { encode, decode, plan } from "../lib/spectro.mjs";
import { testcard, psnr } from "../lib/image.mjs";

const img = testcard(256, 128);
const rows = [];

for (const n of [1024, 2048, 4096])
  for (const div of [2, 4])
    for (const spacing of [2, 4, 6, 8, 12]) {
      const opts = { n, hop: n / div, spacing };
      const p = plan(opts);
      if (p.rows < img.height) continue;
      const { samples } = encode(img, opts);
      const back = decode(samples, {
        width: img.width,
        height: img.height,
        ...opts,
      });
      rows.push({
        n,
        hop: n / div,
        spacing,
        rows: p.rows,
        colRate: p.colRate,
        cells: Math.round(p.rows * p.colRate),
        psnr: psnr(img, back),
      });
    }

rows.sort((a, b) => b.psnr - a.psnr);
console.log("  fft   hop  sp   rows  col/s   cells/s   psnr");
for (const r of rows)
  console.log(
    `${String(r.n).padStart(5)} ${String(r.hop).padStart(5)} ${String(r.spacing).padStart(3)} ` +
      `${String(r.rows).padStart(6)} ${r.colRate.toFixed(1).padStart(6)} ${String(r.cells).padStart(9)}   ${r.psnr.toFixed(2)}`,
  );
