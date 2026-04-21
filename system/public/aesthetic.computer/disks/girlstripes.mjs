// girlstripes, 26.04.21

const { floor, sin, cos, abs, PI, random } = Math;

const count = 14;
let frame = 0;

// Y2K hot pink palette: bubblegum, magenta, lime, baby blue, silver, white
const palette = [
  [255, 20, 147],  // hot pink
  [255, 0, 200],   // magenta
  [255, 105, 180], // bubblegum pink
  [200, 0, 255],   // purple flash
  [0, 255, 180],   // cyber mint
  [180, 255, 0],   // acid lime
  [0, 200, 255],   // baby blue
  [255, 255, 255], // chrome white
  [255, 180, 220], // pastel pink
  [255, 60, 100],  // strawberry
];

const offsets = Array.from({ length: count }, () => random() * 1000);

function paint({ wipe, ink, screen: { width: w, height: h } }) {
  wipe(10, 0, 20);
  frame += 1;

  const baseW = w / count;

  for (let i = 0; i < count; i++) {
    const off = offsets[i];

    // Stripes bounce and shimmy — perky, energetic
    const bounce = sin(frame * 0.05 + off * 0.9) * 0.5 + cos(frame * 0.03 + i) * 0.3;
    const stripeW = floor(baseW * (0.6 + abs(bounce) * 0.8)) + 1;

    // Shimmy side to side fast
    const shimmy = sin(frame * 0.04 + off * 1.1) * baseW * 0.6;
    const x = floor(i * baseW + shimmy);

    // Fast color cycling — y2k energy
    const colorPhase = (frame * 0.02 + off * 0.5) % palette.length;
    const ci = floor(colorPhase) % palette.length;
    const ci2 = (ci + 1) % palette.length;
    const t = colorPhase - floor(colorPhase);
    const [r1, g1, b1] = palette[ci];
    const [r2, g2, b2] = palette[ci2];
    const r = floor(r1 + (r2 - r1) * t);
    const g = floor(g1 + (g2 - g1) * t);
    const b = floor(b1 + (b2 - b1) * t);

    // Bright pulse — glittery
    const pulse = abs(sin(frame * 0.08 + off * 3.1));
    const alpha = 160 + floor(pulse * 95);

    ink(r, g, b, alpha).box(x, 0, stripeW, h, "fill");

    // Sparkle highlight on leading edge
    const sparkle = abs(sin(frame * 0.2 + off * 5.7));
    if (sparkle > 0.85) {
      ink(255, 255, 255, floor(sparkle * 200)).box(x, 0, 2, h, "fill");
    }
  }

  // Glitter dots scattered across screen
  const glitterSeed = frame * 7;
  for (let g = 0; g < 18; g++) {
    const gx = floor((sin(glitterSeed * 0.017 + g * 1.3) * 0.5 + 0.5) * w);
    const gy = floor((cos(glitterSeed * 0.013 + g * 2.1) * 0.5 + 0.5) * h);
    const ga = floor(abs(sin(frame * 0.3 + g * 0.7)) * 200);
    ink(255, 255, 255, ga).box(gx, gy, 2, 2, "fill");
  }
}

function meta() {
  return { title: "Girlstripes", desc: "Y2K hot pink stripes." };
}

export { paint, meta };
