// Halley, 2026.3.02
// Halley's method fractal for f(z) = z³ + 7.

/* 📝 Engineering Notes
  Each pixel maps to a point z₀ in the complex plane.
  Halley's iteration: z ← z - z(z³+7) / (2z³-7)
  Color by which cube root of -7 the orbit converges to.
  Drag to pan. Use the on-screen buttons to zoom and reset.
*/

const { floor, min, cos, sin, PI } = Math;

// View parameters
let cx = 0, cy = 0, zoom = 2.5;
let maxIter = 24;
let dirty = true;

// Drag state
let dragging = false, dragX = 0, dragY = 0, dragCx = 0, dragCy = 0;

// UI buttons
let zoomInBtn, zoomOutBtn, resetBtn;

// The three cube roots of -7: 7^(1/3) · e^(i(π+2πk)/3), k=0,1,2
const cbrt7 = Math.cbrt(7);
const roots = [
  [cbrt7 * cos(PI / 3), cbrt7 * sin(PI / 3)],
  [-cbrt7, 0],
  [cbrt7 * cos(5 * PI / 3), cbrt7 * sin(5 * PI / 3)],
];

// Root colors — three distinct hues
const palette = [
  [60, 180, 255],  // blue
  [255, 80, 120],  // rose
  [80, 255, 160],  // mint
];

function boot({ ui, screen }) {
  zoomInBtn = new ui.TextButton("zoom +", { bottom: 8, right: 8, screen });
  zoomOutBtn = new ui.TextButton("zoom -", { bottom: 8, right: 80, screen });
  resetBtn = new ui.TextButton("reset", { top: 8, right: 8, screen });
}

function paint({ screen, ink }) {
  const { width: w, height: h, pixels } = screen;

  if (dirty) {
    dirty = false;
    const scale = zoom / min(w, h);

    for (let py = 0; py < h; py++) {
      for (let px = 0; px < w; px++) {
        let zr = (px - w / 2) * scale * 2 + cx;
        let zi = (py - h / 2) * scale * 2 + cy;

        let iter = 0;
        let converged = -1;

        for (; iter < maxIter; iter++) {
          const r2 = zr * zr, i2 = zi * zi;
          if (r2 + i2 < 1e-12) break;

          const z2r = r2 - i2;
          const z2i = 2 * zr * zi;
          const z3r = z2r * zr - z2i * zi;
          const z3i = z2r * zi + z2i * zr;

          const fr = z3r + 7;
          const fi = z3i;
          const gr = 2 * z3r - 7;
          const gi = 2 * z3i;

          const nr = zr * fr - zi * fi;
          const ni = zr * fi + zi * fr;

          const denom = gr * gr + gi * gi;
          if (denom < 1e-14) break;

          zr -= (nr * gr + ni * gi) / denom;
          zi -= (ni * gr - nr * gi) / denom;

          for (let k = 0; k < 3; k++) {
            const dr = zr - roots[k][0];
            const di = zi - roots[k][1];
            if (dr * dr + di * di < 1e-6) {
              converged = k;
              break;
            }
          }
          if (converged >= 0) break;
        }

        const idx = (py * w + px) * 4;

        if (converged >= 0) {
          const t = 1 - iter / maxIter;
          const bright = 0.3 + 0.7 * t * t;
          const c = palette[converged];
          pixels[idx] = floor(c[0] * bright);
          pixels[idx + 1] = floor(c[1] * bright);
          pixels[idx + 2] = floor(c[2] * bright);
        } else {
          pixels[idx] = 8;
          pixels[idx + 1] = 6;
          pixels[idx + 2] = 12;
        }
        pixels[idx + 3] = 255;
      }
    }
  }

  // Draw buttons over the fractal
  zoomInBtn.reposition({ bottom: 8, right: 8, screen }, "zoom +");
  zoomOutBtn.reposition({ bottom: 8, right: 80, screen }, "zoom -");
  resetBtn.reposition({ top: 8, right: 8, screen }, "reset");

  const normal = ["white", "black", "black", "white"];
  const hover = ["yellow", "black", "black", "yellow"];

  zoomInBtn.paint({ ink }, normal, hover);
  zoomOutBtn.paint({ ink }, normal, hover);
  resetBtn.paint({ ink }, normal, hover);
}

function act({ event: e, screen, pens }) {
  const zoomFactor = 1.3;

  zoomInBtn.act(e, {
    down: () => { zoom /= zoomFactor; dirty = true; },
  }, pens?.());

  zoomOutBtn.act(e, {
    down: () => { zoom *= zoomFactor; dirty = true; },
  }, pens?.());

  resetBtn.act(e, {
    down: () => { cx = 0; cy = 0; zoom = 2.5; dirty = true; },
  }, pens?.());

  // Drag to pan
  if (e.is("touch")) {
    dragging = true;
    dragX = e.x; dragY = e.y;
    dragCx = cx; dragCy = cy;
  }
  if (e.is("draw") && dragging) {
    const scale = (zoom * 2) / min(screen.width, screen.height);
    cx = dragCx - (e.x - dragX) * scale;
    cy = dragCy - (e.y - dragY) * scale;
    dirty = true;
  }
  if (e.is("lift")) {
    dragging = false;
  }
}

export { boot, paint, act };
export const noBios = true;
