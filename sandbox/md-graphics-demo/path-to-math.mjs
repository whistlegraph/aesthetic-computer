// path-to-math.mjs — rasterize point-sets into an INLINE math block (no image).
//
// Each point becomes a zero-width \rlap-positioned colored \rule. \hspace sets x,
// \raise sets y → the math block is an absolute-positioning canvas. No arrays
// (GitHub's MathJax choked on the array `@{}` form), so this is just a flat list
// of \rlap{...} plus a trailing \phantom strut to claim the W×H box.
//
//   node path-to-math.mjs smiley > snippet.math
//   node path-to-math.mjs flower > snippet.math

const W = 160, H = 160, DOT = 5;

function circle(cx, cy, r, n, col) {
  const pts = [];
  for (let i = 0; i < n; i++) {
    const a = (i / n) * Math.PI * 2;
    pts.push([cx + Math.cos(a) * r, cy + Math.sin(a) * r, col]);
  }
  return pts;
}
function arc(cx, cy, r, a0, a1, n, col) {
  const pts = [];
  for (let i = 0; i < n; i++) {
    const a = a0 + (a1 - a0) * (i / (n - 1));
    pts.push([cx + Math.cos(a) * r, cy + Math.sin(a) * r, col]);
  }
  return pts;
}
function dot(x, y, col) { return [[x, y, col]]; }

const SHAPES = {
  smiley() {
    const Y = "#e8ff8a";
    return [
      ...circle(80, 80, 64, 96, Y),            // face ring
      ...dot(56, 64, Y), ...dot(58, 64, Y),    // left eye
      ...dot(104, 64, Y), ...dot(102, 64, Y),  // right eye
      ...arc(80, 88, 34, 0.25 * Math.PI, 0.75 * Math.PI, 24, Y), // smile
    ];
  },
  flower() {
    const pts = [];
    const petal = "#ff8ad0", core = "#e8ff8a", stem = "#9bd64a";
    for (let k = 0; k < 6; k++) {
      const a = (k / 6) * Math.PI * 2;
      pts.push(...circle(80 + Math.cos(a) * 30, 60 + Math.sin(a) * 30, 18, 36, petal));
    }
    pts.push(...circle(80, 60, 14, 28, core));
    for (let y = 78; y < 150; y += DOT) pts.push([80, y, stem]); // stem
    pts.push(...arc(80, 120, 22, -0.1 * Math.PI, 0.6 * Math.PI, 10, stem)); // leaf
    return pts;
  },
};

const shape = process.argv[2] || "smiley";
const pts = (SHAPES[shape] || SHAPES.smiley)();

const segs = pts.map(([x, y, col]) => {
  const hx = Math.max(0, Math.round(x - DOT / 2));
  // y grows downward in our coords; \raise lifts up, so invert against H.
  const ry = Math.round(H - y - DOT / 2);
  return `\\rlap{\\hspace{${hx}px}\\raise ${ry}px {\\textcolor{${col}}{\\rule{${DOT}px}{${DOT}px}}}}`;
});

process.stdout.write(segs.join("\n") + `\n\\phantom{\\rule{${W}px}{${H}px}}\n`);
