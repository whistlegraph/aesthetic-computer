// Humanoid, 2026.9.23
// The blocky person `arena` and `lairk` both draw: torso, head with a visor
// on its face, arms and legs, each face shaded from one handle color.
// (Lifted from arena's buildRemoteBody so the two worlds share one body.)

/* 📝 Notes
  Body coordinates (arena's convention):
    y = +0.35 head top, y = 0 shoulder/eye, y = -1.1 hip, y = -2.0 feet.
    +Z is forward — the visor faces +Z at yaw 0.
  Colors are 0–1 RGBA per vertex, ready for `new Form({ type: "triangle",
  positions, colors }, …)`.
 */

export const HUMANOID_FEET = -2.0; // Lowest y.
export const HUMANOID_TOP = 0.35; // Highest y.
// The torso box, [xMin, yMin, zMin, xMax, yMax, zMax] — exported so a caller
// can leave it out and dress it separately (lairk paints it).
export const HUMANOID_TORSO = [-0.3, -1.1, -0.18, 0.3, -0.4, 0.18];

// Returns { positions, colors } for one person in `colorRGB` (0–255).
// `watcher` draws a translucent, cooler-visored ghost; `torso: false` skips
// the torso box.
export function humanoid(colorRGB, { watcher = false, torso = true } = {}) {
  const [R, G, B] = colorRGB;
  const a = watcher ? 0.55 : 1.0;
  const r = R / 255, g = G / 255, b = B / 255;
  // Tone tiers from the one handle color.
  const main = [r, g, b, a];
  const dark = [r * 0.55, g * 0.55, b * 0.55, a];
  const light = [Math.min(1, r * 1.18), Math.min(1, g * 1.18), Math.min(1, b * 1.18), a];
  const skinHead = [
    Math.min(1, r * 0.4 + 0.5),
    Math.min(1, g * 0.4 + 0.45),
    Math.min(1, b * 0.4 + 0.4),
    a,
  ];
  const visor = watcher
    ? [0.55, 0.6, 0.7, a]
    : [0.08, 0.08, 0.12, a];

  const positions = [];
  const colors = [];
  // Push one axis-aligned box (in body-local coords). `faces` can override
  // each face individually; missing faces fall back to `main`.
  const pushBox = (xMin, yMin, zMin, xMax, yMax, zMax, faces) => {
    const top = faces.top ?? faces.main;
    const bot = faces.bot ?? faces.main;
    const north = faces.north ?? faces.side ?? faces.main; // -Z
    const south = faces.south ?? faces.side ?? faces.main; // +Z (face/forward)
    const east  = faces.east  ?? faces.side ?? faces.main; // +X
    const west  = faces.west  ?? faces.side ?? faces.main; // -X
    const v = (x, y, z) => [x, y, z, 1];
    const quad = (A, B, C, D, c) => {
      positions.push(A, B, C, A, C, D);
      for (let i = 0; i < 6; i++) colors.push(c);
    };
    quad(v(xMin,yMax,zMin), v(xMin,yMax,zMax), v(xMax,yMax,zMax), v(xMax,yMax,zMin), top);
    quad(v(xMin,yMin,zMin), v(xMax,yMin,zMin), v(xMax,yMin,zMax), v(xMin,yMin,zMax), bot);
    quad(v(xMin,yMin,zMin), v(xMin,yMax,zMin), v(xMax,yMax,zMin), v(xMax,yMin,zMin), north);
    quad(v(xMax,yMin,zMax), v(xMax,yMax,zMax), v(xMin,yMax,zMax), v(xMin,yMin,zMax), south);
    quad(v(xMax,yMin,zMin), v(xMax,yMax,zMin), v(xMax,yMax,zMax), v(xMax,yMin,zMax), east);
    quad(v(xMin,yMin,zMax), v(xMin,yMax,zMax), v(xMin,yMax,zMin), v(xMin,yMin,zMin), west);
  };

  // Torso — primary handle color.
  if (torso) {
    pushBox(...HUMANOID_TORSO, {
      main, top: light, bot: dark, north: dark, south: main,
    });
  }

  // Head — neutral skin tone with a darker visor on the front (+Z) so the
  // facing direction reads at a glance.
  pushBox(-0.22, -0.05, -0.20, 0.22, 0.35, 0.20, {
    main: skinHead,
    top: [
      Math.min(1, skinHead[0] * 1.05),
      Math.min(1, skinHead[1] * 1.05),
      Math.min(1, skinHead[2] * 1.05),
      a,
    ],
    bot: dark,
    south: visor, // +Z = face
  });

  // Arms — thin boxes on either side of the torso.
  pushBox(-0.50, -1.05, -0.12, -0.32, -0.40, 0.12, {
    main: dark, top: main, bot: dark,
  });
  pushBox( 0.32, -1.05, -0.12,  0.50, -0.40, 0.12, {
    main: dark, top: main, bot: dark,
  });

  // Legs — squarer boxes from hip to feet.
  pushBox(-0.22, -2.00, -0.14, -0.04, -1.10, 0.14, {
    main: dark, top: main, bot: [0, 0, 0, a],
  });
  pushBox( 0.04, -2.00, -0.14,  0.22, -1.10, 0.14, {
    main: dark, top: main, bot: [0, 0, 0, a],
  });

  return { positions, colors };
}
