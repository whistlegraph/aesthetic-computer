// KidLisp Mini Renderer — Software Pixel Buffer
// Direct canvas manipulation with Bresenham line and midpoint circle
// ~150 lines, produces ~3-4 KB minified

class Renderer {
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.pixels = new Uint8ClampedArray(width * height * 4);
    this.imageData = new ImageData(this.pixels, width, height);

    // Current color state
    this.r = 255;
    this.g = 255;
    this.b = 255;
    this.a = 255;

    // Transform stacks
    this.transforms = []; // { type, value }
    this.fade = null;
    this.fadeFrame = 0;
  }

  setColor(r, g, b, a = 255) {
    this.r = Math.round(r);
    this.g = Math.round(g);
    this.b = Math.round(b);
    this.a = Math.round(a);
  }

  // Clear entire buffer with current color
  wipe(r, g, b, a = 255) {
    const [cr, cg, cb, ca] = Array.isArray(r) ? r : [r, g, b, a];
    const rgba = [(cr & 255), (cg & 255), (cb & 255), (ca & 255)];

    for (let i = 0; i < this.pixels.length; i += 4) {
      this.pixels[i] = rgba[0];
      this.pixels[i + 1] = rgba[1];
      this.pixels[i + 2] = rgba[2];
      this.pixels[i + 3] = rgba[3];
    }
  }

  // Bresenham line algorithm
  line(x0, y0, x1, y1) {
    x0 = Math.round(x0);
    y0 = Math.round(y0);
    x1 = Math.round(x1);
    y1 = Math.round(y1);

    const dx = Math.abs(x1 - x0);
    const dy = Math.abs(y1 - y0);
    const sx = x0 < x1 ? 1 : -1;
    const sy = y0 < y1 ? 1 : -1;
    let err = dx - dy;

    let x = x0, y = y0;

    while (true) {
      this.plot(x, y);
      if (x === x1 && y === y1) break;

      const e2 = 2 * err;
      if (e2 > -dy) {
        err -= dy;
        x += sx;
      }
      if (e2 < dx) {
        err += dx;
        y += sy;
      }
    }
  }

  // Midpoint circle algorithm
  circle(cx, cy, r, mode = 'fill') {
    cx = Math.round(cx);
    cy = Math.round(cy);
    r = Math.round(r);

    if (mode === 'fill') {
      // Filled circle
      for (let y = -r; y <= r; y++) {
        const yy = cy + y;
        if (yy < 0 || yy >= this.height) continue;

        const x = Math.sqrt(r * r - y * y);
        const x0 = Math.round(cx - x);
        const x1 = Math.round(cx + x);

        for (let xx = x0; xx <= x1; xx++) {
          if (xx >= 0 && xx < this.width) {
            this.plot(xx, yy);
          }
        }
      }
    } else {
      // Outlined circle using midpoint
      let x = 0;
      let y = r;
      let d = 3 - 2 * r;

      while (x <= y) {
        this.plot(cx + x, cy + y);
        this.plot(cx - x, cy + y);
        this.plot(cx + x, cy - y);
        this.plot(cx - x, cy - y);
        this.plot(cx + y, cy + x);
        this.plot(cx - y, cy + x);
        this.plot(cx + y, cy - x);
        this.plot(cx - y, cy - x);

        if (d < 0) {
          d = d + 4 * x + 6;
        } else {
          d = d + 4 * (x - y) + 10;
          y--;
        }
        x++;
      }
    }
  }

  // Box (filled or outlined)
  box(x, y, w, h, mode = 'fill') {
    x = Math.round(x);
    y = Math.round(y);
    w = Math.round(w);
    h = Math.round(h);

    if (mode === 'fill') {
      for (let yy = y; yy < y + h; yy++) {
        if (yy < 0 || yy >= this.height) continue;
        for (let xx = x; xx < x + w; xx++) {
          if (xx >= 0 && xx < this.width) {
            this.plot(xx, yy);
          }
        }
      }
    } else {
      // Outline: four sides
      this.line(x, y, x + w, y); // top
      this.line(x + w, y, x + w, y + h); // right
      this.line(x + w, y + h, x, y + h); // bottom
      this.line(x, y + h, x, y); // left
    }
  }

  // Single pixel
  plot(x, y) {
    x = Math.round(x);
    y = Math.round(y);

    if (x < 0 || x >= this.width || y < 0 || y >= this.height) return;

    const idx = (y * this.width + x) * 4;
    this.pixels[idx] = this.r;
    this.pixels[idx + 1] = this.g;
    this.pixels[idx + 2] = this.b;
    this.pixels[idx + 3] = this.a;
  }

  // Scroll (shift buffer in-place)
  scroll(dx, dy) {
    dx = Math.round(dx);
    dy = Math.round(dy);

    const copy = new Uint8ClampedArray(this.pixels);
    this.pixels.fill(0);

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const srcX = x - dx;
        const srcY = y - dy;

        if (srcX >= 0 && srcX < this.width && srcY >= 0 && srcY < this.height) {
          const srcIdx = (srcY * this.width + srcX) * 4;
          const dstIdx = (y * this.width + x) * 4;

          this.pixels[dstIdx] = copy[srcIdx];
          this.pixels[dstIdx + 1] = copy[srcIdx + 1];
          this.pixels[dstIdx + 2] = copy[srcIdx + 2];
          this.pixels[dstIdx + 3] = copy[srcIdx + 3];
        }
      }
    }
  }

  // Spin (rotate buffer around center)
  spin(angle) {
    const copy = new Uint8ClampedArray(this.pixels);
    this.pixels.fill(0);

    const cx = this.width / 2;
    const cy = this.height / 2;
    const cos = Math.cos(angle);
    const sin = Math.sin(angle);

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        // Rotate backwards to find source pixel
        const dx = x - cx;
        const dy = y - cy;

        const srcX = cx + (dx * cos + dy * sin);
        const srcY = cy + (-dx * sin + dy * cos);

        const sx = Math.round(srcX);
        const sy = Math.round(srcY);

        if (sx >= 0 && sx < this.width && sy >= 0 && sy < this.height) {
          const srcIdx = (sy * this.width + sx) * 4;
          const dstIdx = (y * this.width + x) * 4;

          this.pixels[dstIdx] = copy[srcIdx];
          this.pixels[dstIdx + 1] = copy[srcIdx + 1];
          this.pixels[dstIdx + 2] = copy[srcIdx + 2];
          this.pixels[dstIdx + 3] = copy[srcIdx + 3];
        }
      }
    }
  }

  // Zoom (scale buffer)
  zoom(factor) {
    const copy = new Uint8ClampedArray(this.pixels);
    this.pixels.fill(0);

    const cx = this.width / 2;
    const cy = this.height / 2;

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const dx = (x - cx) / factor;
        const dy = (y - cy) / factor;

        const srcX = Math.round(cx + dx);
        const srcY = Math.round(cy + dy);

        if (srcX >= 0 && srcX < this.width && srcY >= 0 && srcY < this.height) {
          const srcIdx = (srcY * this.width + srcX) * 4;
          const dstIdx = (y * this.width + x) * 4;

          this.pixels[dstIdx] = copy[srcIdx];
          this.pixels[dstIdx + 1] = copy[srcIdx + 1];
          this.pixels[dstIdx + 2] = copy[srcIdx + 2];
          this.pixels[dstIdx + 3] = copy[srcIdx + 3];
        }
      }
    }
  }

  // Contrast
  contrast(factor) {
    const mid = 128;
    for (let i = 0; i < this.pixels.length; i += 4) {
      this.pixels[i] = Math.max(0, Math.min(255, mid + (this.pixels[i] - mid) * factor));
      this.pixels[i + 1] = Math.max(0, Math.min(255, mid + (this.pixels[i + 1] - mid) * factor));
      this.pixels[i + 2] = Math.max(0, Math.min(255, mid + (this.pixels[i + 2] - mid) * factor));
    }
  }

  // Fade background (gradient cycling through colors)
  fadeBackground(colors, frame) {
    const idx = Math.floor(frame / 20) % colors.length; // cycle through colors
    const color = colors[idx];

    if (Array.isArray(color)) {
      this.wipe(color[0], color[1], color[2], color[3]);
    }
  }

  // Present to canvas
  present(ctx) {
    this.imageData.data.set(this.pixels);
    ctx.putImageData(this.imageData, 0, 0);
  }
}

function makeRenderer(width, height) {
  return new Renderer(width, height);
}

export { Renderer, makeRenderer };
