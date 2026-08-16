// picture → audio → picture, with nothing in between. proves the loop and
// gives the ceiling every degraded run gets measured against.

import { encode, decode, plan } from "../lib/spectro.mjs";
import { load, save, psnr, testcard } from "../lib/image.mjs";
import * as wav from "../lib/wav.mjs";

const args = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = args.indexOf("--" + name);
  return i < 0 ? fallback : args[i + 1];
};

const mode = flag("mode", "gray");
const img = args[0] && !args[0].startsWith("--") ? load(args[0]) : testcard();
const out = flag("out", "tapes/out");

const { samples, plan: p, cols } = encode(img, { mode });
wav.write(`${out}/card.wav`, samples, p.rate);

const back = decode(samples, { width: img.width, height: img.height, mode });
save(`${out}/card-decoded.png`, back);
save(`${out}/card-source.png`, img);

const secs = samples.length / p.rate;
console.log(`🖼  ${img.width}×${img.height} ${mode}`);
console.log(`🎞  ${cols} columns · ${p.colRate.toFixed(1)} col/sec · ${secs.toFixed(2)}s`);
console.log(`📻 band ${p.loHz}–${p.hiHz}Hz · ${p.rows} rows available · ${p.range}dB range`);
console.log(`🔍 sync found first column at sample ${back.start}`);
console.log(`📊 psnr ${psnr(img, back).toFixed(2)} dB`);
