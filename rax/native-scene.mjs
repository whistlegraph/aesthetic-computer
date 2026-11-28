// 🥽 The room as a list of boxes, for the native headset app (rax/quest/native):
// one a line, "x y z w h d rrggbb" in metres and rax's own axes, served at
// /native-scene.txt. The app asks every few seconds and makes its picture
// again when the list changes, so what is written here is in the headset as
// soon as the server has it: no new build of the app.
//
// For now this says by hand what view.mjs draws for a roof room (its
// buildArena): the roof and its fire escape, the tower under it with its
// windows, the street and the buildings round it, the clock tower, the
// billboard's building. The two should come to be one list; until then a
// change to the roof there wants the same change here.
import { WORLD, SCREEN, PLATFORM_DEPTH, TOWER_DROP, ESCAPE_DROP, ESCAPE_WIDE, ESCAPE_STEPS, ESCAPE_RAIL, ESCAPE_RUN, CLOCK } from './world.mjs';

export function nativeScene(half = WORLD, now = new Date()) {
  const out = [], hex = (c) => c.toString(16).padStart(6, '0'), r = (n) => Math.round(n * 1000) / 1000;
  const box = (x, y, z, w, h, d, c) => out.push(`${r(x)} ${r(y)} ${r(z)} ${r(w)} ${r(h)} ${r(d)} ${hex(c)}`);
  const W = half, drop = TOWER_DROP, iron = 0x3a3f4d, rail = 0x8b90a0;
  // The roof, and dots on it every two metres to walk by.
  box(0, -PLATFORM_DEPTH / 2, 0, 2 * W, PLATFORM_DEPTH, 2 * W, 0x4a4f5e);
  for (let x = -W + 2; x < W; x += 2) for (let z = -W + 2; z < W; z += 2) box(x, 0.01, z, 0.08, 0.02, 0.08, 0x7a8094);
  // The fire escape: landing, rail and posts, and the steps on the east and west.
  const out2 = W + ESCAPE_WIDE, top = -ESCAPE_DROP;
  for (const [x, z, w, d] of [[(W + out2) / 2, 0, ESCAPE_WIDE, 2 * out2], [-(W + out2) / 2, 0, ESCAPE_WIDE, 2 * out2], [0, (W + out2) / 2, 2 * W, ESCAPE_WIDE], [0, -(W + out2) / 2, 2 * W, ESCAPE_WIDE]]) box(x, top - 0.1, z, w, 0.2, d, iron);
  for (const [x, z, w, d] of [[out2 - 0.05, 0, 0.06, 2 * out2], [-out2 + 0.05, 0, 0.06, 2 * out2], [0, out2 - 0.05, 2 * out2, 0.06], [0, -out2 + 0.05, 2 * out2, 0.06]]) box(x, top + ESCAPE_RAIL, z, w, 0.06, d, rail);
  for (let n = -out2; n <= out2 + 0.01; n += 1.4) for (const [x, z] of [[out2 - 0.05, n], [-out2 + 0.05, n], [n, out2 - 0.05], [n, -out2 + 0.05]]) box(x, top + ESCAPE_RAIL / 2, z, 0.06, ESCAPE_RAIL, 0.06, rail);
  const rise = ESCAPE_DROP / ESCAPE_STEPS;
  for (const side of [1, -1]) for (let k = 0; k < ESCAPE_STEPS; k++) box(side * (W + 0.65), top + ((k + 1) * rise) / 2, side * (-2 + (k + 0.5) * ESCAPE_RUN), 1.3, (k + 1) * rise, ESCAPE_RUN, k % 2 ? 0x4a5061 : iron);
  // The tower under the roof, a band of windows a storey, some lit.
  const shaft = 2 * W - 1.2, under = -0.06 - PLATFORM_DEPTH;
  box(0, under - (drop + under) / 2, 0, shaft, drop + under, shaft, 0x2a2f3c);
  for (let y = under - 2; y > -drop + 2; y -= 3.4) for (let n = 0; n < 4; n++) {
    const across = n % 2 === 0, face = (n < 2 ? 1 : -1) * (shaft / 2 + 0.02);
    for (let k = -2; k <= 2; k++) {
      const at = (k * shaft) / 5.4, lit = (((Math.sin(y * 12.9898 + (n * 7 + k) * 78.233) * 43758.5453) % 1) + 1) % 1 > 0.6;
      if (across) box(at, y, face, shaft / 7, 1.5, 0.04, lit ? 0xffe9a8 : 0x1c2440); else box(face, y, at, 0.04, 1.5, shaft / 7, lit ? 0xffe9a8 : 0x1c2440);
    }
  }
  // The street and the lower buildings.
  box(0, -drop - 0.5, 0, 420, 1, 420, 0x1b1e26);
  for (const [x, z, w, h] of [[-46, -30, 22, 70], [38, -52, 26, 96], [58, 24, 20, 48], [-34, 46, 30, 84], [8, 64, 18, 36], [-72, 8, 24, 58], [84, -8, 22, 104], [-12, -70, 26, 62]]) {
    box(x, -drop + h / 2, z, w, h, w, 0x303646);
    box(x, -drop + h + 0.2, z, w - 2, 0.4, w - 2, 0x3a4154);
  }
  // The clock tower, its face and marks, and its hands as they stand this minute (the list is made again each time it is asked for).
  const c = CLOCK, wide = 2 * c.r + 2.4, front = c.z - wide / 2 - 0.05;
  box(c.x, (c.top - drop) / 2, c.z, wide, c.top + drop, wide, 0x5b6172);
  box(c.x, c.top + 0.6, c.z, wide + 1, 1.2, wide + 1, 0x70778a);
  box(c.x, c.top + 3.6, c.z, 1.2, 5, 1.2, 0x8b90a0);
  box(c.x, c.y, front, 2 * c.r, 2 * c.r, 0.1, 0xf2ecd8);
  for (let h = 0; h < 12; h++) { const a = (h / 12) * Math.PI * 2, far = c.r * 0.86, big = h % 3 === 0 ? 0.34 : 0.18; box(c.x + Math.sin(a) * far, c.y + Math.cos(a) * far, front - 0.06, big, big, 0.06, 0x10131c); }
  // (A hand is a row of small blocks out from the middle: boxes do not turn. Seen from the roof, clockwise is toward -x.)
  const hand = (turns, length, size, color) => { const a = turns * Math.PI * 2; for (let t = 0.1; t <= 1.001; t += 0.1) box(c.x - Math.sin(a) * length * t, c.y + Math.cos(a) * length * t, front - 0.1, size, size, 0.05, color); };
  const m = now.getMinutes(), h = (now.getHours() % 12) + m / 60;
  hand(h / 12, c.r * 0.5, 0.3, 0x10131c);
  hand(m / 60, c.r * 0.78, 0.22, 0x10131c);
  // The billboard's building and its frame (the picture itself is the page's KidLisp: not here yet).
  box(-SCREEN - 6.2, (22 - drop) / 2, 0, 12, 22 + drop, 36, 0x303646);
  box(-SCREEN - 0.3, 10.5, 0, 0.3, 19.4, 33.4, 0x10131c);
  box(-SCREEN - 0.1, 10.5, 0, 0.1, 18, 32, 0xb8d94a);
  return `# rax native scene: ${out.length} boxes, ${now.toISOString()}\n${out.join('\n')}\n`;
}
