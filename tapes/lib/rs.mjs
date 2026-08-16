// reed-solomon over GF(256), systematic, with interleaving.
//
// this is for the *program* tape, not the picture. a picture wants to degrade
// into grain; a piece of source code wants to be right or known-wrong. the
// numbers say the same: spending this channel on 4-bit pixels plus parity is
// about five times worse than spending it on analog greys, and it fails to a
// blank screen instead of a noisy one.

const { min, floor } = Math;

const EXP = new Uint8Array(512),
  LOG = new Uint8Array(256);
{
  let x = 1;
  for (let i = 0; i < 255; i += 1) {
    EXP[i] = x;
    LOG[x] = i;
    x <<= 1;
    if (x & 0x100) x ^= 0x11d; // the usual primitive polynomial
  }
  for (let i = 255; i < 512; i += 1) EXP[i] = EXP[i - 255];
}

const mul = (a, b) => (a === 0 || b === 0 ? 0 : EXP[LOG[a] + LOG[b]]);
const div = (a, b) => (a === 0 ? 0 : EXP[LOG[a] + 255 - LOG[b]]);

function polyMul(a, b) {
  const out = new Uint8Array(a.length + b.length - 1);
  for (let i = 0; i < a.length; i += 1)
    for (let j = 0; j < b.length; j += 1) out[i + j] ^= mul(a[i], b[j]);
  return out;
}

function polyEval(p, x) {
  let y = p[0];
  for (let i = 1; i < p.length; i += 1) y = mul(y, x) ^ p[i];
  return y;
}

function generator(nsym) {
  let g = Uint8Array.from([1]);
  for (let i = 0; i < nsym; i += 1) g = polyMul(g, Uint8Array.from([1, EXP[i]]));
  return g;
}

// ── one block ─────────────────────────────────────────────────────────────

export function encodeBlock(msg, nsym) {
  const g = generator(nsym);
  const out = new Uint8Array(msg.length + nsym);
  out.set(msg);
  for (let i = 0; i < msg.length; i += 1) {
    const c = out[i];
    if (c === 0) continue;
    for (let j = 1; j < g.length; j += 1) out[i + j] ^= mul(g[j], c);
  }
  out.set(msg); // the division scribbled over the message; put it back
  return out;
}

// returns { data, fixed } or null when there are more errors than parity
// can locate — a clean failure, which is the point of using it at all.
export function decodeBlock(block, nsym) {
  const r = Uint8Array.from(block);
  const synd = new Uint8Array(nsym);
  let bad = false;
  for (let i = 0; i < nsym; i += 1) {
    synd[i] = polyEval(r, EXP[i]);
    if (synd[i]) bad = true;
  }
  if (!bad) return { data: r.subarray(0, r.length - nsym), fixed: 0 };

  const sigma = berlekampMassey(synd, nsym);
  const pos = chien(sigma, r.length);
  if (pos.length === 0 || pos.length > floor(nsym / 2)) return null;
  if (!forney(r, synd, sigma, pos, nsym)) return null;

  // re-check: a mis-correction usually leaves syndromes non-zero
  for (let i = 0; i < nsym; i += 1) if (polyEval(r, EXP[i])) return null;
  return { data: r.subarray(0, r.length - nsym), fixed: pos.length };
}

function berlekampMassey(synd, nsym) {
  let sigma = [1],
    old = [1];
  for (let i = 0; i < nsym; i += 1) {
    old = [...old, 0];
    let delta = synd[i];
    for (let j = 1; j < sigma.length; j += 1)
      delta ^= mul(sigma[sigma.length - 1 - j], synd[i - j]);
    if (delta !== 0) {
      if (old.length > sigma.length) {
        const scaled = old.map((v) => mul(v, delta));
        old = sigma.map((v) => mul(v, EXP[255 - LOG[delta]]));
        sigma = scaled;
      }
      const next = [...sigma];
      const shift = old.length - sigma.length;
      for (let j = 0; j < old.length; j += 1) {
        const k = j - shift;
        if (k >= 0) next[k] ^= mul(old[j], delta);
      }
      sigma = next;
    }
  }
  return sigma;
}

// roots of sigma give the error positions
function chien(sigma, len) {
  const pos = [];
  for (let i = 0; i < len; i += 1)
    if (polyEval(sigma, EXP[255 - i]) === 0) pos.push(len - 1 - i);
  return pos;
}

function forney(r, synd, sigma, pos, nsym) {
  // omega = synd * sigma mod x^nsym
  const rev = Uint8Array.from(synd).reverse();
  const omegaFull = polyMul(rev, Uint8Array.from(sigma));
  const omega = omegaFull.subarray(omegaFull.length - nsym);

  for (const p of pos) {
    const xi = EXP[(r.length - 1 - p) % 255];
    const xiInv = EXP[255 - LOG[xi]];
    const num = polyEval(omega, xiInv);
    const den = derivEval(sigma, xiInv);
    if (den === 0) return false;
    r[p] ^= mul(xi, div(num, den));
  }
  return true;
}

function derivEval(sigma, x) {
  // odd-degree terms survive differentiation in characteristic 2
  const deg = sigma.length - 1;
  let acc = 0;
  for (let i = 0; i <= deg; i += 1) {
    const power = deg - i;
    if (power % 2 === 1) acc ^= mul(sigma[i], powX(x, power - 1));
  }
  return acc;
}

const powX = (x, e) => (e === 0 ? 1 : x === 0 ? 0 : EXP[(LOG[x] * e) % 255]);

// ── framed, interleaved ───────────────────────────────────────────────────

// tape damage arrives in bursts and the packing spreads one bad tone across a
// run of bytes, so blocks are interleaved: consecutive bytes on the wire come
// from different blocks, and a burst becomes one error in each.
export function protect(bytes, { nsym = 32, k = 223 } = {}) {
  const blocks = [];
  for (let i = 0; i < bytes.length; i += k) {
    const chunk = new Uint8Array(k);
    chunk.set(bytes.subarray(i, min(i + k, bytes.length)));
    blocks.push(encodeBlock(chunk, nsym));
  }
  const width = k + nsym;
  const out = new Uint8Array(blocks.length * width);
  for (let b = 0; b < blocks.length; b += 1)
    for (let j = 0; j < width; j += 1) out[j * blocks.length + b] = blocks[b][j];
  return { bytes: out, blocks: blocks.length, len: bytes.length, nsym, k };
}

export function recover(wire, { blocks, len, nsym = 32, k = 223 } = {}) {
  const width = k + nsym;
  const out = new Uint8Array(blocks * k);
  let fixed = 0,
    lost = 0;
  for (let b = 0; b < blocks; b += 1) {
    const block = new Uint8Array(width);
    for (let j = 0; j < width; j += 1) block[j] = wire[j * blocks + b] ?? 0;
    const got = decodeBlock(block, nsym);
    if (!got) {
      lost += 1;
      out.set(block.subarray(0, k), b * k); // hand back what arrived
    } else {
      fixed += got.fixed;
      out.set(got.data, b * k);
    }
  }
  return { bytes: out.subarray(0, len), fixed, lost, blocks };
}
