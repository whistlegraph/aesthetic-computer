const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const lerp = (a, b, t) => a + (b - a) * t;

function hslToRgb(h, s, l) {
  const hue = ((h % 360) + 360) % 360;
  const chroma = (1 - Math.abs(2 * l - 1)) * s;
  const segment = hue / 60;
  const x = chroma * (1 - Math.abs((segment % 2) - 1));
  let r = 0;
  let g = 0;
  let b = 0;

  if (segment >= 0 && segment < 1) {
    r = chroma;
    g = x;
  } else if (segment < 2) {
    r = x;
    g = chroma;
  } else if (segment < 3) {
    g = chroma;
    b = x;
  } else if (segment < 4) {
    g = x;
    b = chroma;
  } else if (segment < 5) {
    r = x;
    b = chroma;
  } else {
    r = chroma;
    b = x;
  }

  const m = l - chroma / 2;
  return [
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255),
  ];
}

function createBuffer(width, height) {
  const pixels = new Uint8ClampedArray(width * height * 4);
  for (let i = 0; i < width * height; i += 1) {
    pixels[i * 4 + 3] = 255;
  }
  return pixels;
}

function setPixel(buffer, width, height, x, y, r, g, b, alpha = 1) {
  if (x < 0 || y < 0 || x >= width || y >= height) return;
  const offset = (Math.floor(y) * width + Math.floor(x)) * 4;
  const mix = clamp(alpha, 0, 1);
  buffer[offset] = Math.round(lerp(buffer[offset], clamp(r, 0, 255), mix));
  buffer[offset + 1] = Math.round(lerp(buffer[offset + 1], clamp(g, 0, 255), mix));
  buffer[offset + 2] = Math.round(lerp(buffer[offset + 2], clamp(b, 0, 255), mix));
  buffer[offset + 3] = 255;
}

function fillRect(buffer, width, height, x, y, w, h, color, alpha = 1) {
  const startX = clamp(Math.floor(x), 0, width);
  const startY = clamp(Math.floor(y), 0, height);
  const endX = clamp(Math.ceil(x + w), 0, width);
  const endY = clamp(Math.ceil(y + h), 0, height);
  for (let py = startY; py < endY; py += 1) {
    for (let px = startX; px < endX; px += 1) {
      setPixel(buffer, width, height, px, py, color[0], color[1], color[2], alpha);
    }
  }
}

function fillCircle(buffer, width, height, cx, cy, radius, color, alpha = 1) {
  const startX = clamp(Math.floor(cx - radius), 0, width);
  const startY = clamp(Math.floor(cy - radius), 0, height);
  const endX = clamp(Math.ceil(cx + radius), 0, width);
  const endY = clamp(Math.ceil(cy + radius), 0, height);
  const radiusSq = radius * radius;

  for (let y = startY; y < endY; y += 1) {
    for (let x = startX; x < endX; x += 1) {
      const dx = x + 0.5 - cx;
      const dy = y + 0.5 - cy;
      if (dx * dx + dy * dy <= radiusSq) {
        setPixel(buffer, width, height, x, y, color[0], color[1], color[2], alpha);
      }
    }
  }
}

function drawLine(buffer, width, height, x0, y0, x1, y1, color, alpha = 1) {
  const steps = Math.max(1, Math.ceil(Math.hypot(x1 - x0, y1 - y0)));
  for (let step = 0; step <= steps; step += 1) {
    const t = step / steps;
    const x = lerp(x0, x1, t);
    const y = lerp(y0, y1, t);
    setPixel(buffer, width, height, x, y, color[0], color[1], color[2], alpha);
  }
}

function paintBackgroundGradient(buffer, width, height, baseHue, hueSpread, lightness = 0.2) {
  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const hue = baseHue + (x / Math.max(1, width - 1) - 0.5) * hueSpread + (y / Math.max(1, height - 1) - 0.5) * hueSpread * 0.4;
      const l = clamp(lightness + Math.sin((x + y) * 0.06) * 0.04, 0.05, 0.75);
      const color = hslToRgb(hue, 0.72, l);
      setPixel(buffer, width, height, x, y, color[0], color[1], color[2], 1);
    }
  }
}

const fixtures = {
  "pulse-square": {
    name: "pulse-square",
    description: "A pulsing central square with shifting nested color bands.",
    render({ width, height, frame, frames }) {
      const pixels = createBuffer(width, height);
      const t = frame / Math.max(1, frames);
      const baseHue = 25 + Math.sin(frame * 0.08) * 90;
      paintBackgroundGradient(pixels, width, height, baseHue, 110, 0.14 + Math.sin(frame * 0.04) * 0.03);

      const pulse = 0.5 + 0.5 * Math.sin(frame * 0.22);
      const squareSize = Math.max(8, Math.floor(Math.min(width, height) * (0.18 + pulse * 0.22)));
      const inset = Math.max(4, Math.floor(squareSize * 0.18));
      const x = width / 2 - squareSize / 2;
      const y = height / 2 - squareSize / 2;
      const outer = hslToRgb(baseHue + 140, 0.88, 0.58);
      const inner = hslToRgb(baseHue + 240, 0.84, 0.7);
      const core = hslToRgb(baseHue + 320, 0.9, 0.82);

      fillRect(pixels, width, height, x, y, squareSize, squareSize, outer, 0.9);
      fillRect(pixels, width, height, x + inset, y + inset, squareSize - inset * 2, squareSize - inset * 2, inner, 0.92);
      fillRect(pixels, width, height, x + inset * 2, y + inset * 2, squareSize - inset * 4, squareSize - inset * 4, core, 0.95);

      const cross = hslToRgb(baseHue + 45, 0.7, 0.55);
      drawLine(pixels, width, height, 0, height / 2, width, height / 2, cross, 0.35 + pulse * 0.25);
      drawLine(pixels, width, height, width / 2, 0, width / 2, height, cross, 0.35 + (1 - pulse) * 0.2);

      return pixels;
    },
  },
  "gradient-sweep": {
    name: "gradient-sweep",
    description: "A full-frame spectrum gradient with drifting bands and diagonal sweep.",
    render({ width, height, frame, frames }) {
      const pixels = createBuffer(width, height);
      const t = frame / Math.max(1, frames);
      for (let y = 0; y < height; y += 1) {
        for (let x = 0; x < width; x += 1) {
          const nx = x / Math.max(1, width - 1);
          const ny = y / Math.max(1, height - 1);
          const hue = 360 * nx + frame * 2.6 + Math.sin(ny * 8 + frame * 0.09) * 28;
          const sat = clamp(0.6 + Math.sin((nx - ny) * 6 + frame * 0.05) * 0.18, 0.25, 0.95);
          const light = clamp(0.22 + ny * 0.45 + Math.sin((nx + ny) * 14 + frame * 0.07) * 0.08, 0.08, 0.86);
          const color = hslToRgb(hue, sat, light);
          setPixel(pixels, width, height, x, y, color[0], color[1], color[2], 1);
        }
      }

      const bandHue = 200 + Math.sin(frame * 0.13) * 120;
      const bandColor = hslToRgb(bandHue, 0.88, 0.8);
      const sweepX = (t * width * 1.6) % (width * 1.6) - width * 0.3;
      drawLine(pixels, width, height, sweepX, 0, sweepX - width * 0.35, height, bandColor, 0.35);
      drawLine(pixels, width, height, sweepX + width * 0.12, 0, sweepX - width * 0.23, height, bandColor, 0.22);
      return pixels;
    },
  },
  "orbit-blobs": {
    name: "orbit-blobs",
    description: "Three orbiting blobs with connecting lines and a luminous center.",
    render({ width, height, frame }) {
      const pixels = createBuffer(width, height);
      const baseHue = 200 + Math.sin(frame * 0.04) * 40;
      paintBackgroundGradient(pixels, width, height, baseHue, 60, 0.1);

      const cx = width / 2;
      const cy = height / 2;
      const orbits = [
        { radius: Math.min(width, height) * 0.28, speed: 0.09, hue: baseHue + 140, size: 0.1 },
        { radius: Math.min(width, height) * 0.22, speed: -0.12, hue: baseHue + 260, size: 0.08 },
        { radius: Math.min(width, height) * 0.16, speed: 0.17, hue: baseHue + 20, size: 0.07 },
      ];

      const points = orbits.map((orbit, index) => {
        const angle = frame * orbit.speed + index * Math.PI * 0.66;
        return {
          x: cx + Math.cos(angle) * orbit.radius,
          y: cy + Math.sin(angle * 1.08) * orbit.radius,
          color: hslToRgb(orbit.hue, 0.9, 0.62),
          radius: Math.max(4, Math.floor(Math.min(width, height) * orbit.size)),
        };
      });

      const spine = hslToRgb(baseHue + 80, 0.65, 0.66);
      for (const point of points) {
        drawLine(pixels, width, height, cx, cy, point.x, point.y, spine, 0.28);
        fillCircle(pixels, width, height, point.x, point.y, point.radius, point.color, 0.92);
      }
      fillCircle(pixels, width, height, cx, cy, Math.max(5, Math.floor(Math.min(width, height) * 0.06)), hslToRgb(baseHue + 300, 0.95, 0.82), 0.96);

      return pixels;
    },
  },
};

export const SONIC_FIXTURES = Object.freeze(fixtures);

export function listSonicFixtures() {
  return Object.keys(SONIC_FIXTURES);
}

export function getSonicFixture(name) {
  return SONIC_FIXTURES[name] || null;
}
