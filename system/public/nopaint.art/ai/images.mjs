export const SIZE = 256;
export function canvas() {
  const c = document.createElement("canvas");
  c.width = c.height = SIZE;
  return c;
}
export async function read(url) {
  const image = new Image();
  image.src = url;
  await image.decode();
  const c = canvas();
  c.getContext("2d").drawImage(image, 0, 0, SIZE, SIZE);
  return c;
}
export function random(seed) {
  let n = seed >>> 0;
  return () => {
    n += 0x6d2b79f5;
    let t = n;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
export function noise(seed) {
  const c = canvas(),
    ctx = c.getContext("2d"),
    pixels = ctx.createImageData(SIZE, SIZE),
    rng = random(seed);
  for (let i = 0; i < pixels.data.length; i += 4) {
    pixels.data[i] = rng() * 256;
    pixels.data[i + 1] = rng() * 256;
    pixels.data[i + 2] = rng() * 256;
    pixels.data[i + 3] = 255;
  }
  ctx.putImageData(pixels, 0, 0);
  return c.toDataURL();
}
export const FAMILIES = {
  "classic-pixels": ["slice", "turn", "sort", "mosaic"],
  "classic-color": ["hue", "tint", "contrast", "posterize"],
  "classic-primitives": ["stroke", "shapes", "stipple", "hatch", "diffuse"],
};
FAMILIES.classic = Object.values(FAMILIES).flat();
export async function classic(
  url,
  { engine = "classic", seed, strength = 0.25 },
  preview = () => {},
) {
  const c = await read(url),
    ctx = c.getContext("2d", { willReadFrequently: true }),
    rng = random(seed),
    pick = (a) => a[Math.floor(rng() * a.length)];
  const family = FAMILIES[engine];
  if (!family) throw Error("Unknown local palette.");
  const op = pick(family),
    level = [0.25, 0.5, 0.75].indexOf(strength),
    count = [3, 5, 8][level],
    side = [32, 64, 112][level];
  const color = () =>
    `rgb(${Math.floor(rng() * 256)} ${Math.floor(rng() * 256)} ${Math.floor(rng() * 256)})`;
  const point = () => Math.floor(rng() * SIZE);
  for (let step = 0; step < count; step++) {
    const x = Math.floor(rng() * (SIZE - side)),
      y = Math.floor(rng() * (SIZE - side));
    if (op === "slice") {
      const from = ctx.getImageData(0, y, SIZE, Math.max(1, side / 4)),
        shift = Math.floor(rng() * side) + 1;
      ctx.putImageData(from, shift, y);
      ctx.putImageData(from, shift - SIZE, y);
    } else if (op === "turn") {
      const tile = canvas();
      tile.width = tile.height = side;
      tile.getContext("2d").drawImage(c, x, y, side, side, 0, 0, side, side);
      ctx.save();
      ctx.translate(x + side / 2, y + side / 2);
      ctx.rotate(Math.PI / 2);
      ctx.drawImage(tile, -side / 2, -side / 2);
      ctx.restore();
    } else if (op === "mosaic") {
      const tile = canvas();
      tile.width = tile.height = Math.max(2, side / 8);
      tile
        .getContext("2d")
        .drawImage(c, x, y, side, side, 0, 0, tile.width, tile.height);
      ctx.imageSmoothingEnabled = false;
      ctx.drawImage(tile, x, y, side, side);
    } else if (
      ["sort", "hue", "tint", "contrast", "posterize", "diffuse"].includes(op)
    ) {
      const patch = ctx.getImageData(x, y, side, side),
        d = patch.data,
        original = d.slice(),
        tint = [point(), point(), point()];
      if (op === "sort")
        for (let row = 0; row < side; row++) {
          const pixels = Array.from({ length: side }, (_, col) =>
            Array.from(
              d.slice((row * side + col) * 4, (row * side + col) * 4 + 4),
            ),
          );
          pixels.sort((a, b) => a[0] + a[1] + a[2] - (b[0] + b[1] + b[2]));
          pixels.forEach((p, col) => d.set(p, (row * side + col) * 4));
        }
      else
        for (let i = 0; i < d.length; i += 4)
          for (let k = 0; k < 3; k++) {
            if (op === "hue") d[i + k] = original[i + ((k + 1) % 3)];
            if (op === "tint") d[i + k] = d[i + k] * 0.75 + tint[k] * 0.25;
            if (op === "contrast")
              d[i + k] = (d[i + k] - 128) * (1.1 + strength) + 128;
            if (op === "posterize")
              d[i + k] =
                Math.round(d[i + k] / [32, 64, 85][level]) *
                [32, 64, 85][level];
            if (op === "diffuse") {
              const neighbor =
                Math.max(
                  0,
                  Math.min(
                    d.length / 4 - 1,
                    i / 4 + pick([-side, -1, 1, side]),
                  ),
                ) * 4;
              d[i + k] = (original[i + k] + original[neighbor + k]) / 2;
            }
          }
      ctx.putImageData(patch, x, y);
    } else {
      ctx.fillStyle = ctx.strokeStyle = color();
      ctx.lineWidth = [2, 5, 10][level];
      if (op === "shapes") {
        if (rng() < 0.5) ctx.fillRect(x, y, side, side);
        else {
          ctx.beginPath();
          ctx.ellipse(
            x + side / 2,
            y + side / 2,
            side / 2,
            side / 2,
            0,
            0,
            Math.PI * 2,
          );
          ctx.fill();
        }
      }
      if (op === "stroke") {
        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.bezierCurveTo(
          point(),
          point(),
          point(),
          point(),
          x + side,
          y + side,
        );
        ctx.stroke();
      }
      if (op === "stipple")
        for (let j = 0; j < 60; j++)
          ctx.fillRect(x + rng() * side, y + rng() * side, 2, 2);
      if (op === "hatch")
        for (let j = 0; j < side; j += 8) {
          ctx.beginPath();
          ctx.moveTo(x + j, y);
          ctx.lineTo(x + j, y + side);
          ctx.stroke();
        }
    }
    await preview(c.toDataURL());
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
  if (c.toDataURL() === url) {
    ctx.fillStyle = color();
    ctx.fillRect(point(), point(), 8, 8);
  }
  return {
    image: c.toDataURL(),
    operation: op,
    engine_version: "browser-classic-v1",
    seed,
  };
}
export async function crop(url, box) {
  const source = await read(url),
    c = canvas();
  c.getContext("2d").imageSmoothingEnabled = false;
  c.getContext("2d").drawImage(
    source,
    box[0],
    box[1],
    box[2] - box[0],
    box[3] - box[1],
    0,
    0,
    SIZE,
    SIZE,
  );
  return c.toDataURL();
}
export async function region(url, mask) {
  if (!mask?.some(Boolean))
    return { input: url, finish: async (image) => image };
  let x0 = SIZE,
    y0 = SIZE,
    x1 = 0,
    y1 = 0;
  mask.forEach((v, i) => {
    if (v) {
      x0 = Math.min(x0, i % SIZE);
      y0 = Math.min(y0, Math.floor(i / SIZE));
      x1 = Math.max(x1, (i % SIZE) + 1);
      y1 = Math.max(y1, Math.floor(i / SIZE) + 1);
    }
  });
  const side = Math.min(SIZE, Math.max(x1 - x0, y1 - y0) + 16),
    x = Math.max(0, Math.min(SIZE - side, Math.floor((x0 + x1 - side) / 2))),
    y = Math.max(0, Math.min(SIZE - side, Math.floor((y0 + y1 - side) / 2)));
  const input = await crop(url, [x, y, x + side, y + side]);
  return {
    input,
    async finish(image) {
      const c = await read(url),
        ctx = c.getContext("2d"),
        original = ctx.getImageData(0, 0, SIZE, SIZE),
        generated = await read(image);
      ctx.drawImage(generated, x, y, side, side);
      const out = ctx.getImageData(0, 0, SIZE, SIZE);
      for (let i = 0; i < mask.length; i++)
        if (!mask[i])
          out.data.set(original.data.subarray(i * 4, i * 4 + 4), i * 4);
      ctx.putImageData(out, 0, 0);
      return c.toDataURL();
    },
  };
}
export function maskBits(mask) {
  if (!mask?.some(Boolean)) return null;
  const bytes = new Uint8Array((SIZE * SIZE) / 8);
  mask.forEach((v, i) => {
    if (v) bytes[i >> 3] |= 128 >> i % 8;
  });
  return btoa(String.fromCharCode(...bytes));
}
export function readMask(bits) {
  const mask = new Uint8Array(SIZE * SIZE);
  if (bits) {
    const raw = atob(bits);
    for (let i = 0; i < mask.length; i++)
      mask[i] = raw.charCodeAt(i >> 3) & (128 >> i % 8) ? 1 : 0;
  }
  return mask;
}
export async function pngBytes(url) {
  return new Uint8Array(await (await fetch(url)).arrayBuffer());
}
export function download(data, name) {
  const url = typeof data === "string" ? data : URL.createObjectURL(data),
    a = document.createElement("a");
  a.href = url;
  a.download = name;
  a.click();
  if (typeof data !== "string")
    setTimeout(() => URL.revokeObjectURL(url), 1000);
}
