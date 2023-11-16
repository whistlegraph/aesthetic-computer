// Sno, 2023.11.15.12.10.43.464
// A snowball game by @ida, @mxsage and @jeffrey.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [x] Get proper rotation mapped to a sprite sheet.
    - [x] Transcribe @mxsage's rotation mapping from C++.  
#endregion */

const { min, floor, abs } = Math;

const ball = {
  x: 0,
  y: 0,
  px: 0,
  py: 0,
  xvel: 0,
  yvel: 0,
  dec: 0.97,
  xang: 0,
  yang: 0,
  radius: undefined,
  axis: undefined,
  rot: undefined,
};

let ballSheet;
let disc;

let LEFT,
  RIGHT,
  UP,
  DOWN = false;

// 🥾 Boot
function boot({ wipe, num: { quat } }) {
  // Runs once at the start.
  ball.rot = quat.setAxisAngle(quat.create(), [1, 0, 0], 0);
}

// 🎨 Paint
function paint({ screen, wipe, ink, pan, unpan, write, paste, num }) {
  const short = min(screen.width, screen.height); // Longest view w/ margin.
  const cam = { x: screen.width / 2, y: screen.height / 2, scale: 1 };

  pan(cam.x, cam.y);

  // 🥏 Playground Disc
  disc = { x: 0, y: 0, radius: short / 2.5 };
  wipe(64)
    .ink(255, 25) // Snow disc filled,
    .circle(disc.x, disc.y, cam.scale * disc.radius, true, 1, 1)
    .ink(255) // w/ outline.
    .circle(disc.x, disc.y, cam.scale * disc.radius, false, 1, 1);

  // ⚾ Snowball
  const b = ball;
  b.radius = disc.radius / 3;
  ink(255, 64).circle(b.x, b.y, b.radius, true, 1, 0.1);
  ink(255, 128).circle(b.x, b.y, b.radius + 1, false, 1, 0.01);

  // Draw snowball frame...
  if (ballSheet) {
    const rows = 12;
    const tile = ballSheet.width / rows;
    let tx = floor(b.xang);
    let ty = floor(b.yang);
    if (tx < 0) tx = rows + tx;
    if (ty < 0) ty = rows + ty;
    tx = rows - 1 - tx;

    paste(
      {
        painting: ballSheet,
        crop: { x: tx * tile, y: ty * tile, w: tile, h: tile },
      },
      b.x - tile / 2,
      b.y - tile / 2,
    );
  }

  if (b.axis) {
    ink(255, 64).line(
      b.x,
      b.y,
      b.x + b.axis[0] * b.radius,
      b.y + b.axis[1] * b.radius,
    );
  }

  if (b.up) {
    ink(255, 0, 0, 255 * b.up[2]).box(
      b.x + b.up[0] * b.radius,
      b.y + b.up[1] * b.radius,
      4,
      "center",
    );
    ink(127, 32).line(
      b.x,
      b.y,
      b.x + b.up[0] * b.radius,
      b.y + b.up[2] * b.radius,
    );
  }

  unpan();

  // 🧮 Data
  ink("yellow").write(`xang: ${num.radians(b.xang)}`, { x: 6, y: 18 });
  ink("yellow").write(`yang: ${num.radians(b.yang)}`, { x: 6, y: 18 + 11 });
}

// 🧮 Sim
function sim({ num: { p2, vec3, vec4, quat, mat4 } }) {
  if (!disc) return;

  const step = 0.055;

  // Accelerate the ball as needed.
  if (LEFT) ball.xvel -= step;
  if (RIGHT) ball.xvel += step;
  if (UP) ball.yvel -= step;
  if (DOWN) ball.yvel += step;

  ball.px = ball.x;
  ball.py = ball.y;
  ball.x += ball.xvel;
  ball.y += ball.yvel;

  // Check for any collision with the edges, and slow down the ball if needed.
  if (p2.dist(ball, disc) > disc.radius) {
    ball.xvel *= ball.dec / 1.1;
    ball.yvel *= ball.dec / 1.1;
  }

  ball.xang = (ball.xang + ball.xvel) % 360;
  ball.yang = (ball.yang + ball.yvel) % 360;

  const dist = step * p2.dist(ball, { x: ball.px, y: ball.py });

  // Set ball axis.
  if (dist > 0.001) {
    ball.axis = vec3.cross(
      vec3.create(),
      vec3.fromValues(0, 0, 1),
      vec3.normalize(
        vec3.create(),
        vec3.subtract(
          vec3.create(),
          vec3.fromValues(ball.px, ball.py, 0),
          vec3.fromValues(ball.x, ball.y, 0),
        ),
      ),
    );

    ball.rot = quat.multiply(
      quat.create(),
      quat.setAxisAngle(quat.create(), ball.axis, dist),
      ball.rot,
    );

    const normedRotation = quat.normalize(quat.create(), ball.rot);
    const m4 = mat4.fromQuat(mat4.create(), normedRotation);

    const up = vec4.transformMat4(
      vec4.create(),
      vec4.fromValues(0, 0, 1, 1),
      m4,
    );

    ball.up = up;
  } else {
    ball.axis = undefined;
  }

  // Apply decceleration no matter what.
  ball.xvel *= ball.dec;
  ball.yvel *= ball.dec;
}

// 🎪 Act
function act({ event: e }) {
  // Respond to user input here.
  if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) LEFT = true;
  if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) LEFT = false;

  if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) RIGHT = true;
  if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) RIGHT = false;

  if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup")) UP = true;
  if (e.is("keyboard:up:w") || e.is("keyboard:up:arrowup")) UP = false;

  if (e.is("keyboard:down:s") || e.is("keyboard:down:arrowdown")) DOWN = true;
  if (e.is("keyboard:up:s") || e.is("keyboard:up:arrowdown")) DOWN = false;

  if (e.is("dropped:bitmap")) ballSheet = e.painting; // Load a spritesheet.
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Sno",
    desc: "A snowball game by @ida, @mxsage and @jeffrey.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, sim, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
