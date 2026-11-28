// 🥽 Who is in the room this instant, as a list of boxes, for the native
// headset app (rax/quest/native): the same lines as native-scene.mjs
// ("x y z w h d rrggbb", metres, rax's own axes), served at /native-live.txt
// and asked for about ten times a second. The app draws it over the room, so
// the people and the heads are seen to move there.
//
// A body is legs, a chest in its seat's colour, a head, and a small block
// before the face to say which way it looks (boxes do not turn). A flying
// head is a red box with a dark one before it. Rounds and things lying about
// are small blocks. With no room made yet (nobody has come), only the note.
import { COLORS } from './world.mjs';

const EYE = 1.7; // (a player's y is its eye: the feet are this far under)
const dim = (c, k) => (Math.round(((c >> 16) & 255) * k) << 16) | (Math.round(((c >> 8) & 255) * k) << 8) | Math.round((c & 255) * k);

export function nativeLive(match) {
  const out = [], hex = (c) => c.toString(16).padStart(6, '0'), r = (n) => Math.round(n * 1000) / 1000;
  const box = (x, y, z, w, h, d, c) => { if ([x, y, z].every(Number.isFinite)) out.push(`${r(x)} ${r(y)} ${r(z)} ${r(w)} ${r(h)} ${r(d)} ${hex(c)}`); };
  const ahead = (yaw, far) => { const a = ((yaw || 0) * Math.PI) / 180; return [Math.sin(a) * far, Math.cos(a) * far]; };
  let people = 0;
  (match?.seats || []).forEach((s, seat) => {
    const p = s?.player;
    if (!p || p.alive === false) return;
    people++;
    const c = COLORS[seat % COLORS.length], feet = p.y - EYE, [fx, fz] = ahead(p.yaw, 0.2);
    box(p.x, feet + 0.45, p.z, 0.36, 0.9, 0.36, dim(c, 0.45));
    box(p.x, feet + 1.15, p.z, 0.52, 0.5, 0.4, c);
    box(p.x, feet + 1.62, p.z, 0.34, 0.34, 0.34, 0xf2e0c8);
    box(p.x + fx, feet + 1.64, p.z + fz, 0.2, 0.1, 0.2, 0x10131c);
  });
  for (const h of match?.heads || []) {
    const [fx, fz] = ahead(h.yaw, 0.36);
    box(h.x, h.y, h.z, 0.7, 0.7, 0.7, 0xe0241c);
    box(h.x + fx, h.y + 0.08, h.z + fz, 0.5, 0.16, 0.12, 0x200404);
  }
  for (const b of match?.bullets || []) box(b.x, b.y, b.z, 0.14, 0.14, 0.14, 0xfff2a0);
  for (const it of match?.items?.values?.() || []) if (!(it.holder >= 0)) box(it.x, it.y, it.z, 0.3, 0.2, 0.3, 0xf4f4f4);
  return `# rax native live: ${out.length} boxes, ${people} people, ${match?.heads?.length || 0} heads, tick ${match?.tick ?? -1}\n${out.join('\n')}${out.length ? '\n' : ''}`;
}
