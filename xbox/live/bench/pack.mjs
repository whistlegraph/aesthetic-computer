// Same projected-vertex contract as OskiewarScene3D, without per-face arrays.
export function pack(frame, target) {
  const t = frame.triangles;
  if (t.length % 12 || target.length < t.length / 12 * 18)
    throw new RangeError('Invalid triangle trace or destination size');
  let at = 0;
  for (let i = 0; i < t.length; i += 12) for (let p = 0; p < 9; p += 3) {
    target[at++] = t[i+p] / 960 - 1;
    target[at++] = 1 - t[i+p+1] / 540;
    target[at++] = Math.max(0, Math.min(1, (t[i+p+2]+1.5)/3));
    target[at++] = t[i+9] / 255; target[at++] = t[i+10] / 255; target[at++] = t[i+11] / 255;
  }
}
