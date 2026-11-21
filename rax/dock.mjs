// 🖥️ Going in to a screen: the sums of it, apart from the page so they can be
// checked (test/dock.test.mjs). A `face` is a flat thing looked at square on:
// { x, y, z } its middle, `yaw` the way it is turned (as a world panel is:
// facing south, -z, until turned), `w` by `h` metres. A `pose` is where an eye
// stands and how it looks: { x, y, z, yaw, pitch, zoom }, the zoom a factor on
// the picture's scale (1: as the room is seen walking about).

// Where the eye stands to have a face fill the picture, and how far the lens
// closes on it. `scale` is the picture's own (view.mjs `scale()`: a thing one
// metre off the middle at one metre away lands `sx` of the half-width across,
// `sy` of the half-height up). It stands back about the face's own size, no
// further than `clear` (the open air in front of it) allows, and the lens
// takes up the rest: whichever of width and height is the tighter fits, with
// `margin` of the picture left around it, so a tall face fills a tall picture
// and a wide one a wide. Null where there is no room to stand.
export function dockPose(face, scale, { margin = 0.08, clear = Infinity } = {}) {
  const a = ((face.yaw || 0) * Math.PI) / 180, back = Math.min(Math.max(face.w, face.h) * 1.2 + 0.6, clear - 0.35);
  if (!(back >= 0.5)) return null;
  const zoom = (1 - margin) * Math.min(back / ((face.w / 2) * scale.sx), back / ((face.h / 2) * scale.sy));
  return { x: face.x - Math.sin(a) * back, y: face.y, z: face.z - Math.cos(a) * back, yaw: (((face.yaw || 0) % 360) + 360) % 360, pitch: 0, zoom: Math.min(16, Math.max(0.5, zoom)), back };
}

// 🚁 Where a drone hovers to look a thing over. A face is seen from its front,
// a little further back than going in stands; a build (`box`, its whole box)
// from the side the drone is already on, back by about its size and a little
// over its middle. `clear` again is the open air that way from the thing.
export function faceStandoff(face, clear = Infinity) {
  const a = ((face.yaw || 0) * Math.PI) / 180, back = Math.min(Math.max(2, Math.max(face.w, face.h) * 1.6 + 1), clear - 0.5);
  return back >= 0.8 ? { x: face.x - Math.sin(a) * back, y: Math.max(0.6, face.y), z: face.z - Math.cos(a) * back } : null;
}
export function boxMiddle(box) {
  return { x: (box.xMin + box.xMax) / 2, y: (box.yMin + box.yMax) / 2, z: (box.zMin + box.zMax) / 2 };
}
// (The way out from the thing to where the drone is, flat; due south if it is right over it.)
export function bearingFrom(box, from) {
  const m = boxMiddle(box), dx = from.x - m.x, dz = from.z - m.z, l = Math.hypot(dx, dz);
  return l > 0.05 ? { x: dx / l, y: 0, z: dz / l } : { x: 0, y: 0, z: -1 };
}
export function boxStandoff(box, from, clear = Infinity) {
  const m = boxMiddle(box), wide = Math.hypot(box.xMax - box.xMin, box.zMax - box.zMin), tall = box.yMax - box.yMin, size = Math.max(wide, tall);
  const out = bearingFrom(box, from), back = Math.min(Math.max(2.5, wide / 2 + size * 1.1), clear - 0.6);
  return back >= wide / 2 + 0.5 ? { x: m.x + out.x * back, y: Math.max(1.2, m.y + tall * 0.25), z: m.z + out.z * back } : null;
}

// A glide from one pose to another, `e` of the way (0 to 1): the heading goes
// the short way round, and the lens closes by ratio, so it reads as steady.
export const smooth = (t) => { const k = Math.min(1, Math.max(0, t)); return k * k * (3 - 2 * k); };
export function glide(a, b, e) {
  const mix = (p, q) => p + (q - p) * e, turn = ((b.yaw - a.yaw + 540) % 360) - 180;
  return { x: mix(a.x, b.x), y: mix(a.y, b.y), z: mix(a.z, b.z), yaw: (a.yaw + turn * e + 360) % 360, pitch: mix(a.pitch || 0, b.pitch || 0), zoom: (a.zoom || 1) * ((b.zoom || 1) / (a.zoom || 1)) ** e };
}

// 🖱️ The mouse on a face gone in to: moved by so many of the face's pixels,
// and stopped at its edges.
export function cursorStep(cursor, face, dx, dy) {
  return { x: Math.min(face.cols - 0.01, Math.max(0, cursor.x + dx)), y: Math.min(face.rows - 0.01, Math.max(0, cursor.y + dy)) };
}

// 🎚️ How fast the eye is changing between two poses `seconds` apart, each way
// it can, 0 to 1: the lens (a doubling a second is all of it), its travel
// (8 metres a second), its turning (120 degrees a second). For the whirr.
export function rates(was, now, seconds) {
  const dt = Math.max(1 / 240, seconds), one = (v) => Math.min(1, v);
  const turned = Math.abs(((now.yaw - was.yaw + 540) % 360) - 180) + Math.abs((now.pitch || 0) - (was.pitch || 0));
  return {
    lens: one(Math.abs(Math.log((now.zoom || 1) / (was.zoom || 1))) / dt / Math.LN2),
    travel: one(Math.hypot(now.x - was.x, now.y - was.y, now.z - was.z) / dt / 8),
    turn: one(turned / dt / 120),
  };
}
