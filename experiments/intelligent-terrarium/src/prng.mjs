import { createHash } from "node:crypto";

function seedWords(seed) {
  const bytes = createHash("sha256").update(String(seed)).digest();
  const words = [];
  for (let i = 0; i < 4; i += 1) words.push(bytes.readUInt32LE(i * 4) || (0x9e3779b9 + i));
  return words;
}

export class Prng {
  constructor(seedOrState) {
    this.words = Array.isArray(seedOrState) ? seedOrState.map((word) => word >>> 0) : seedWords(seedOrState);
    if (this.words.length !== 4) throw new TypeError("PRNG state must contain four words");
    this.calls = 0;
  }

  static fromJSON(state) {
    const prng = new Prng(state.words);
    prng.calls = state.calls >>> 0;
    return prng;
  }

  nextUint() {
    let [x, y, z, w] = this.words;
    const t = (x ^ (x << 11)) >>> 0;
    x = y;
    y = z;
    z = w;
    w = (w ^ (w >>> 19) ^ t ^ (t >>> 8)) >>> 0;
    this.words = [x, y, z, w];
    this.calls = (this.calls + 1) >>> 0;
    return w;
  }

  float() {
    return this.nextUint() / 0x100000000;
  }

  signed() {
    return this.float() * 2 - 1;
  }

  toJSON() {
    return { words: [...this.words], calls: this.calls };
  }
}
