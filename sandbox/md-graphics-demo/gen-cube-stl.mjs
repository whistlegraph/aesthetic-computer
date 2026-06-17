// gen-cube-stl.mjs — emit an ASCII STL from code (no deps).
//
// GitHub renders a committed .stl with an interactive 3D viewer in the file view
// (rotate/zoom). It does NOT reliably embed inside markdown via ![] — you LINK to
// the file and the viewer opens. This is the 3D leg of "draw anything": any mesh
// you can compute, you can commit and spin on github.com.
//
//   node gen-cube-stl.mjs > cube.stl

const s = 10; // half-size
// 8 cube corners.
const v = [
  [-s, -s, -s], [s, -s, -s], [s, s, -s], [-s, s, -s],
  [-s, -s, s], [s, -s, s], [s, s, s], [-s, s, s],
];
// 12 triangles (2 per face), CCW.
const tris = [
  [0, 1, 2], [0, 2, 3], // -z
  [4, 6, 5], [4, 7, 6], // +z
  [0, 4, 5], [0, 5, 1], // -y
  [3, 2, 6], [3, 6, 7], // +y
  [1, 5, 6], [1, 6, 2], // +x
  [0, 3, 7], [0, 7, 4], // -x
];

function normal(a, b, c) {
  const u = [b[0] - a[0], b[1] - a[1], b[2] - a[2]];
  const w = [c[0] - a[0], c[1] - a[1], c[2] - a[2]];
  const n = [u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2], u[0] * w[1] - u[1] * w[0]];
  const len = Math.hypot(...n) || 1;
  return n.map((x) => (x / len).toFixed(4));
}

let out = "solid ac-cube\n";
for (const [i, j, k] of tris) {
  const [a, b, c] = [v[i], v[j], v[k]];
  const n = normal(a, b, c);
  out += `  facet normal ${n[0]} ${n[1]} ${n[2]}\n    outer loop\n`;
  for (const p of [a, b, c]) out += `      vertex ${p[0]} ${p[1]} ${p[2]}\n`;
  out += "    endloop\n  endfacet\n";
}
out += "endsolid ac-cube\n";
process.stdout.write(out);
