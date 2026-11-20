// Scream, 26.11.20
// 😱 The takeover. A scream covers every connected screen: its words, one at
// a time and as large as they fit, in step with the voice when it has one.
// The piece underneath keeps running; its pixels are set aside each frame and
// put back before it paints again, so nothing it drew is lost.

const HOLD = 2500; // How long a scream with no voice holds, in ms.
const TAIL = 350; // What a voiced one holds past its last sound.
const GLYPH = { w: 6, h: 10 };

let on = null; // { text, words, from, until, playing }
let backing = null; // The piece's pixels under the takeover.

export const busy = () => !!(on || backing);

export function hear(text) {
  const from = performance.now();
  on = { text: String(text).toUpperCase(), from, until: from + HOLD };
}

// `vox` is a take from /api/scream. The takeover restarts when its sound does.
export async function voice(vox, { preload, play }) {
  const sfx = await preload(vox.audio);
  on?.playing?.kill?.(0.05);
  const from = performance.now();
  on = { text: vox.text, words: vox.words, from, until: from + vox.duration * 1000 + TAIL, playing: play(sfx) };
}

// Put the piece's pixels back. Called before the piece paints.
export function lift(screen) {
  if (backing?.length === screen.pixels.length) screen.pixels.set(backing);
  backing = null;
}

// Called once the frame's own layers are flushed. True while it covered the
// frame or just let go of it: either way the whole frame has to be sent.
export function paint({ ink, layer, screen }) {
  if (!on) return false;
  const t = performance.now();
  if (t > on.until) {
    on = null;
    return true;
  }
  backing = new Uint8ClampedArray(screen.pixels);

  let word = on.text;
  if (on.words?.length) {
    const s = (t - on.from) / 1000;
    word = (on.words.findLast((w) => w.s <= s) || on.words[0]).w;
  }
  const size = Math.max(1, Math.min(
    Math.floor((screen.width - 12) / (word.length * GLYPH.w)),
    Math.floor((screen.height - 40) / GLYPH.h),
  ));
  // `write` centers on a point, and on an unscaled line height.
  const x = screen.width / 2 + Math.round((Math.random() - 0.5) * size);
  const y = screen.height / 2 - ((size - 1) * GLYPH.h) / 2 + Math.round((Math.random() - 0.5) * size);

  layer(1002);
  ink(200, 0, 0).box(0, 0, screen.width, screen.height); // A steady field: no strobing.
  if (on.words?.length) {
    ink(0).write(word, { center: "xy", size, x: x + size, y: y + size });
    ink("yellow").write(word, { center: "xy", size, x, y });
    ink(255, 190, 190).write(on.text, { center: "x", y: screen.height - 22 }, undefined, screen.width - 12);
  } else {
    ink("yellow").write(on.text, { center: "xy", size: 2 }, undefined, screen.width - 12);
  }
  return true;
}
