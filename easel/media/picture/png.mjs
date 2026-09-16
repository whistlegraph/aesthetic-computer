// Portable bounded PNG codec: 8-bit noninterlaced grayscale/RGB/RGBA.
import { deflateSync, inflateSync } from "node:zlib";
const signature = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]);
function crc(bytes) {
  let c = 0xffffffff;
  for (const b of bytes) {
    c ^= b;
    for (let k = 0; k < 8; k++) c = (c >>> 1) ^ (c & 1 ? 0xedb88320 : 0);
  }
  return (c ^ 0xffffffff) >>> 0;
}
function chunk(type, data) {
  const name = Buffer.from(type),
    out = Buffer.alloc(data.length + 12);
  out.writeUInt32BE(data.length);
  name.copy(out, 4);
  data.copy(out, 8);
  out.writeUInt32BE(crc(Buffer.concat([name, data])), data.length + 8);
  return out;
}
export function dimensions(width, height) {
  if (
    !Number.isInteger(width) ||
    !Number.isInteger(height) ||
    width < 1 ||
    height < 1 ||
    width > 2048 ||
    height > 2048
  )
    throw new Error("Picture dimensions must be 1–2048 pixels.");
}
export function encode({ width, height, data }) {
  dimensions(width, height);
  const head = Buffer.alloc(13);
  head.writeUInt32BE(width);
  head.writeUInt32BE(height, 4);
  head[8] = 8;
  head[9] = 6;
  const raw = Buffer.alloc((width * 4 + 1) * height);
  for (let y = 0; y < height; y++)
    Buffer.from(data.subarray(y * width * 4, (y + 1) * width * 4)).copy(
      raw,
      y * (width * 4 + 1) + 1,
    );
  return Buffer.concat([
    signature,
    chunk("IHDR", head),
    chunk("IDAT", deflateSync(raw)),
    chunk("IEND", Buffer.alloc(0)),
  ]);
}
export function decode(bytes) {
  if (
    bytes.length > 32 * 1024 * 1024 ||
    !bytes.subarray(0, 8).equals(signature)
  )
    throw new Error("Import requires a PNG under 32 MB.");
  let width,
    height,
    channels,
    transparent = false;
  const blocks = [];
  for (let p = 8; p + 12 <= bytes.length; ) {
    const n = bytes.readUInt32BE(p),
      type = bytes.toString("ascii", p + 4, p + 8);
    if (p + n + 12 > bytes.length) throw new Error("Truncated PNG.");
    const b = bytes.subarray(p + 8, p + 8 + n);
    if (crc(bytes.subarray(p + 4, p + 8 + n)) !== bytes.readUInt32BE(p + 8 + n))
      throw new Error("PNG checksum mismatch.");
    if (type === "IHDR") {
      if (n !== 13 || width) throw new Error("Invalid PNG header.");
      width = b.readUInt32BE(0);
      height = b.readUInt32BE(4);
      dimensions(width, height);
      channels = { 0: 1, 2: 3, 4: 2, 6: 4 }[b[9]];
      if (b[8] !== 8 || !channels || b[10] || b[11] || b[12])
        throw new Error("Use an 8-bit noninterlaced RGB/RGBA PNG.");
    } else if (type === "IDAT") blocks.push(b);
    else if (type === "tRNS") transparent = true;
    else if (type === "IEND") break;
    p += n + 12;
  }
  if (!width || !blocks.length || transparent)
    throw new Error("Unsupported PNG; export as RGBA first.");
  const stride = width * channels,
    raw = inflateSync(Buffer.concat(blocks), {
      maxOutputLength: (stride + 1) * height,
    });
  if (raw.length !== (stride + 1) * height)
    throw new Error("PNG pixel length mismatch.");
  const pixels = Buffer.alloc(stride * height),
    data = Buffer.alloc(width * height * 4);
  const paeth = (a, b, c) => {
    const p = a + b - c,
      pa = Math.abs(p - a),
      pb = Math.abs(p - b),
      pc = Math.abs(p - c);
    return pa <= pb && pa <= pc ? a : pb <= pc ? b : c;
  };
  for (let y = 0; y < height; y++) {
    const filter = raw[y * (stride + 1)];
    if (filter > 4) throw new Error("Invalid PNG filter.");
    for (let x = 0; x < stride; x++) {
      const i = y * stride + x,
        a = x >= channels ? pixels[i - channels] : 0,
        b = y ? pixels[i - stride] : 0,
        c = y && x >= channels ? pixels[i - stride - channels] : 0;
      pixels[i] =
        (raw[y * (stride + 1) + 1 + x] +
          [0, a, b, Math.floor((a + b) / 2), paeth(a, b, c)][filter]) &
        255;
    }
  }
  for (let i = 0, j = 0; i < pixels.length; i += channels, j += 4) {
    data[j] = pixels[i];
    data[j + 1] = channels < 3 ? pixels[i] : pixels[i + 1];
    data[j + 2] = channels < 3 ? pixels[i] : pixels[i + 2];
    data[j + 3] =
      channels === 2 ? pixels[i + 1] : channels === 4 ? pixels[i + 3] : 255;
  }
  return { width, height, data };
}
export function composite(target, source, opacity = 1) {
  for (let y = 0; y < target.height; y++)
    for (let x = 0; x < target.width; x++) {
      const i = (y * target.width + x) * 4,
        j =
          (Math.floor((y * source.height) / target.height) * source.width +
            Math.floor((x * source.width) / target.width)) *
          4;
      blend(target.data, i, source.data.subarray(j, j + 4), opacity);
    }
}
export function blend(data, i, color, opacity = 1) {
  const a = (color[3] / 255) * opacity,
    b = data[i + 3] / 255,
    out = a + b * (1 - a);
  if (!out) return;
  for (let c = 0; c < 3; c++)
    data[i + c] = Math.round((color[c] * a + data[i + c] * b * (1 - a)) / out);
  data[i + 3] = Math.round(out * 255);
}
