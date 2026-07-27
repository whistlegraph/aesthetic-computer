// Danz Ballet Studio, 26.07.26.23.42
// An animated opening-night splash page for Danz Ballet Studio.

const { floor, sin, cos, min, max, PI } = Math;

let frame = 0;
let flourish = 0;

function boot({ cursor, hud }) {
  cursor("native");
  hud.labelBack();
}

function sim() {
  frame += 1;
  flourish *= 0.96;
}

function ribbon(ink, line, points, color, alpha = 255) {
  ink(...color, alpha);
  for (let i = 1; i < points.length; i += 1) {
    line(
      floor(points[i - 1][0]),
      floor(points[i - 1][1]),
      floor(points[i][0]),
      floor(points[i][1]),
    );
  }
}

function paint({ wipe, ink, line, screen, circle }) {
  const { width: w, height: h } = screen;
  const t = frame / 120;
  const compact = w < 430 || h < 420;
  const titleSize = compact ? 3 : 4;
  const cx = floor(w / 2);

  // Warm paper, with a barely-there stage glow.
  wipe(248, 242, 235);
  for (let i = 6; i > 0; i -= 1) {
    const pulse = 0.92 + sin(t * 0.6) * 0.04;
    ink(236, 196, 196, 7 + i * 2).circle(
      cx,
      floor(h * 0.47),
      min(w, h) * (0.17 + i * 0.055) * pulse,
      true,
    );
  }

  // A pair of slowly breathing ribbons cross the stage like choreography.
  const ribbons = [
    { phase: 0, color: [151, 43, 65], alpha: 86, y: 0.30 },
    { phase: PI, color: [221, 151, 157], alpha: 100, y: 0.70 },
  ];
  for (const r of ribbons) {
    const points = [];
    const steps = max(32, floor(w / 9));
    for (let i = 0; i <= steps; i += 1) {
      const x = (i / steps) * w;
      const sweep = sin(i * 0.22 + t * 0.45 + r.phase) * h * 0.075;
      const bow = sin((i / steps) * PI) * h * 0.05;
      points.push([x, h * r.y + sweep + (r.phase ? -bow : bow)]);
    }
    ribbon(ink, line, points, r.color, r.alpha);
  }

  // Minimal dancer: a continuous, breathing figure behind the wordmark.
  const dancerY = floor(h * (compact ? 0.27 : 0.29));
  const scale = min(w, h) / 310;
  const sway = sin(t * 0.7) * 3 * scale;
  const rose = [126, 38, 58];
  ink(...rose, 75).circle(cx + sway, dancerY - 36 * scale, 6 * scale, true);
  ink(...rose, 80);
  line(cx + sway, dancerY - 29 * scale, cx - 2 * scale, dancerY + 5 * scale);
  line(cx - 2 * scale, dancerY + 5 * scale, cx - 28 * scale, dancerY + 48 * scale);
  line(cx - 2 * scale, dancerY + 5 * scale, cx + 34 * scale, dancerY + 24 * scale);
  line(cx - 1 * scale, dancerY - 17 * scale, cx - 42 * scale, dancerY - 5 * scale);
  line(cx - 1 * scale, dancerY - 17 * scale, cx + 38 * scale, dancerY - 31 * scale);

  const wordmarkY = floor(h * (compact ? 0.43 : 0.46));
  ink(126, 38, 58).write("DANZ", {
    center: "x",
    y: wordmarkY,
    size: titleSize,
    screen,
  });
  ink(59, 45, 45).write("BALLET STUDIO", {
    center: "x",
    y: wordmarkY + titleSize * 12 + 7,
    size: compact ? 1 : 2,
    screen,
  });

  const ruleY = wordmarkY + titleSize * 12 + (compact ? 24 : 36);
  const ruleHalf = min(w * 0.28, 150);
  ink(126, 38, 58, 105).line(cx - ruleHalf, ruleY, cx + ruleHalf, ruleY);
  ink(126, 38, 58, 180).circle(cx, ruleY, 2 + flourish * 2, true);

  ink(92, 73, 70).write("A NEW SPACE IS TAKING SHAPE", {
    center: "x",
    y: ruleY + 19,
    screen,
  });
  ink(153, 125, 119).write(
    compact ? "CLASSES  ·  SCHEDULE" : "CLASSES  ·  SCHEDULE  ·  REGISTRATION",
    {
      center: "x",
      y: ruleY + 35,
      screen,
    },
  );
  ink(177, 149, 142).write(
    compact ? "REGISTRATION  ·  COMING SOON" : "COMING SOON",
    {
      center: "x",
      y: ruleY + 51,
      screen,
    },
  );

  // Quiet stage line and footlights.
  const stageY = h - max(24, floor(h * 0.075));
  ink(126, 38, 58, 50).line(floor(w * 0.12), stageY, floor(w * 0.88), stageY);
  for (let i = 0; i < 5; i += 1) {
    const x = cx + (i - 2) * min(38, w * 0.075);
    const glow = 90 + floor((sin(t * 1.4 + i) + 1) * 30);
    ink(221, 151, 157, glow).circle(x, stageY, 1.5, true);
  }
}

function act({ event }) {
  if (event.is("touch")) flourish = 1;
}

export { boot, sim, paint, act };
