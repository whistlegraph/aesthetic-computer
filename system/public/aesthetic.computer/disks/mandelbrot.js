// mandelbrot — direct pixel-array fractal. Tests p5's loadPixels()
// round-trip and demonstrates pan/zoom via mouse.

const MAX_ITER = 80;
let centerX = -0.5;
let centerY = 0;
let zoom = 1;
let needsRender = true;

function setup() {
  createCanvas(windowWidth, windowHeight);
  pixelDensity(1);
  noLoop(); // recompute only on input
  renderFractal();
}

function renderFractal() {
  loadPixels();
  const w = width, h = height;
  const span = 3 / zoom;
  const aspect = h / w;
  const minX = centerX - span / 2;
  const minY = centerY - (span * aspect) / 2;
  const stepX = span / w;
  const stepY = (span * aspect) / h;

  for (let py = 0; py < h; py++) {
    const cy = minY + py * stepY;
    for (let px = 0; px < w; px++) {
      const cx = minX + px * stepX;
      let zx = 0, zy = 0;
      let it = 0;
      while (it < MAX_ITER && zx * zx + zy * zy < 4) {
        const t = zx * zx - zy * zy + cx;
        zy = 2 * zx * zy + cy;
        zx = t;
        it++;
      }
      const idx = (py * w + px) * 4;
      if (it === MAX_ITER) {
        pixels[idx] = pixels[idx + 1] = pixels[idx + 2] = 0;
      } else {
        const t = it / MAX_ITER;
        pixels[idx] = (9 * (1 - t) * t * t * t * 255) | 0;
        pixels[idx + 1] = (15 * (1 - t) * (1 - t) * t * t * 255) | 0;
        pixels[idx + 2] = (8.5 * (1 - t) * (1 - t) * (1 - t) * t * 255) | 0;
      }
      pixels[idx + 3] = 255;
    }
  }
  updatePixels();
  needsRender = false;
}

function draw() {
  if (needsRender) renderFractal();
}

function mousePressed() {
  // zoom in centered at click
  const span = 3 / zoom;
  const aspect = height / width;
  centerX = centerX - span / 2 + (mouseX / width) * span;
  centerY = centerY - (span * aspect) / 2 + (mouseY / height) * (span * aspect);
  zoom *= 2.2;
  needsRender = true;
  redraw();
}

function keyPressed() {
  if (key === "r" || key === "R") {
    centerX = -0.5;
    centerY = 0;
    zoom = 1;
    needsRender = true;
    redraw();
  }
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
  needsRender = true;
  redraw();
}
