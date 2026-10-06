// Whistlegraph v2 — a basket of blueberries.
export function paint({ wipe, ink, circle, line, shape, screen }) {
  wipe("#151838");

  const cx = screen.width / 2;
  const rimY = screen.height * 0.56;
  const botY = screen.height * 0.9;
  const topW = Math.min(screen.width * 0.66, 460);
  const botW = topW * 0.68;
  const r = topW * 0.075;

  // basket body
  ink("#7a4a22");
  shape([
    [cx - topW / 2, rimY], [cx + topW / 2, rimY],
    [cx + botW / 2, botY], [cx - botW / 2, botY],
  ], true);

  const half = (t) => (topW + (botW - topW) * t) / 2;
  ink("#96602c");
  for (let i = 1; i < 6; i += 1) {
    const t = i / 6;
    const y = rimY + (botY - rimY) * t;
    line(cx - half(t), y, cx + half(t), y);
  }
  ink("#673d1a");
  for (let k = -3; k <= 3; k += 1) {
    line(cx + (k * topW) / 7, rimY, cx + (k * botW) / 7, botY);
  }

  // the pile — back rows first so nearer berries overlap
  const rows = [
    { n: 5, y: rimY - r * 0.1 },
    { n: 4, y: rimY - r * 1.05 },
    { n: 2, y: rimY - r * 1.85 },
  ];
  rows.forEach(({ n, y }, row) => {
    for (let i = 0; i < n; i += 1) {
      const x = cx + (i - (n - 1) / 2) * r * 1.72;
      berry(ink, circle, line, x + Math.sin(row * 3 + i * 2) * r * 0.12, y, r, row * 2 + i);
    }
  });

  // front rim lip holds them in
  ink("#a56a30");
  line(cx - topW / 2, rimY, cx + topW / 2, rimY, r * 0.45);
  ink("#c98a45");
  line(cx - topW / 2, rimY - r * 0.22, cx + topW / 2, rimY - r * 0.22, r * 0.12);
}

function berry(ink, circle, line, x, y, r, seed) {
  const shades = ["#2b3390", "#3843ad", "#4653c6", "#5866dc"];
  ink(shades[seed % shades.length]);
  circle(x, y, r, true);

  ink("#8a95f6");
  circle(x - r * 0.3, y - r * 0.32, r * 0.34, true);
  ink("#cdd3ff");
  circle(x - r * 0.36, y - r * 0.38, r * 0.12, true);

  // calyx: five star points around a small central stem
  ink("#191c4d");
  const top = y - r * 0.82;
  for (let i = 0; i < 5; i += 1) {
    const a = -Math.PI / 2 + (i - 2) * 0.5;
    line(x, top, x + Math.cos(a) * r * 0.34, top + Math.sin(a) * r * 0.3);
  }
  ink("#232666");
  circle(x, top, r * 0.11, true);
  line(x, top, x - r * 0.05, top - r * 0.15);
}
