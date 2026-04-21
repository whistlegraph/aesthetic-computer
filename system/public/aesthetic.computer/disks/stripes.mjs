// stripes, 26.04.21

const { floor, sin, PI } = Math;
const count = 12;
let frame = 0;

function paint({ wipe, ink, screen: { width: w, height: h } }) {
  wipe(0);
  frame += 1;
  const stripeW = w / count;
  for (let i = 0; i < count; i++) {
    const t = (i / count + frame * 0.004) % 1;
    const r = floor(sin(t * PI * 2) * 127 + 128);
    const g = floor(sin(t * PI * 2 + (PI * 2) / 3) * 127 + 128);
    const b = floor(sin(t * PI * 2 + (PI * 4) / 3) * 127 + 128);
    ink(r, g, b).box(floor(i * stripeW), 0, floor(stripeW) + 1, h, "fill");
  }
}

function meta() {
  return { title: "Stripes", desc: "Cycling color stripes." };
}

export { paint, meta };
