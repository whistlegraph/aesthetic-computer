// png in, png out, plus the one number we judge a round trip by.

import { readFileSync, writeFileSync } from "node:fs";
import { PNG } from "pngjs";

const { log10, round, min, max, sqrt } = Math;

export function load(path) {
  const png = PNG.sync.read(readFileSync(path));
  return { width: png.width, height: png.height, data: png.data };
}

export function save(path, img) {
  const png = new PNG({ width: img.width, height: img.height });
  png.data = Buffer.from(img.data);
  writeFileSync(path, PNG.sync.write(png));
}

export function psnr(a, b) {
  let sum = 0,
    n = 0;
  for (let i = 0; i < a.data.length; i += 4)
    for (let c = 0; c < 3; c += 1) {
      const d = a.data[i + c] - b.data[i + c];
      sum += d * d;
      n += 1;
    }
  const mse = sum / n;
  return mse === 0 ? Infinity : 10 * log10((255 * 255) / mse);
}

// grayscale test card: ramps to see banding, wedges to see how fine a detail
// survives, flats to see noise, edges to see ringing.
export function testcard(w = 256, h = 256) {
  const data = new Uint8Array(w * h * 4).fill(255);
  const set = (x, y, v) => {
    const i = (y * w + x) * 4;
    data[i] = data[i + 1] = data[i + 2] = min(255, max(0, round(v)));
  };
  for (let y = 0; y < h; y += 1)
    for (let x = 0; x < w; x += 1) {
      const u = x / w,
        v = y / h;
      let g;
      if (v < 0.25) g = u * 255; // horizontal ramp
      else if (v < 0.5) {
        const period = 2 + round((1 - u) * 14); // wedge, fine on the right
        g = x % period < period / 2 ? 235 : 20;
      } else if (v < 0.75) {
        const step = round(u * 7) / 7; // stair steps
        g = step * 255;
      } else {
        const c = (x >> 3) + (y >> 3); // checker with a diagonal through it
        g = c % 2 ? 200 : 40;
        if (abs2(x - y) < 2) g = 255;
      }
      set(x, y, g);
    }
  return { width: w, height: h, data };
}

const abs2 = (n) => (n < 0 ? -n : n);
