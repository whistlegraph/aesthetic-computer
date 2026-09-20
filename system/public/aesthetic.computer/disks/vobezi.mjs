// vobezi, 26.09.10
// A red field with a pulsing circle inside a spinning square.

let t = 0;

function paint({ wipe, ink, screen }) {
  t += 1;

  wipe(200, 20, 30);

  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const base = Math.min(screen.width, screen.height) * 0.3;

  // Circle breathes.
  const radius = base * (0.75 + 0.25 * Math.sin(t / 24));
  ink(255, 240, 120).circle(cx, cy, radius, true);

  // Square spins around it.
  const spin = t / 60;
  const reach = base * 1.15;
  const corners = [];
  for (let i = 0; i < 4; i += 1) {
    const a = spin + (i * Math.PI) / 2 + Math.PI / 4;
    corners.push([cx + Math.cos(a) * reach, cy + Math.sin(a) * reach]);
  }
  for (let i = 0; i < 4; i += 1) {
    const [x1, y1] = corners[i];
    const [x2, y2] = corners[(i + 1) % 4];
    ink(30, 30, 60).line(x1, y1, x2, y2);
  }

  ink(60, 0, 10).write("vobezi", { center: "xy" });

  return true; // Keep painting.
}

export { paint };
