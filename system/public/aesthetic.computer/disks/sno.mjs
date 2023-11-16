// Sno, 2023.11.15.12.10.43.464
// A snowball game by @ida, @mxsage and @jeffrey.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add a 16-directionally rotating character.
  - [x] Transcribe @mxsage's rotation mapping from C++.  
  - [-] Get proper rotation mapped to a sprite sheet.
#endregion */

const { min, floor } = Math;

const ball = {
  x: 0,
  y: 0,
  xvel: 0,
  yvel: 0,
  dec: 0.97,
  xang: 0,
  yang: 0,
  radius: undefined,
};

let ballSheet;
let disc;

let LEFT,
  RIGHT,
  UP,
  DOWN = false;

// 🥾 Boot
function boot({ wipe,  }) {
  // Runs once at the start.
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
  ball.radius = disc.radius / 12;
  ink(255, 64).circle(ball.x, ball.y, ball.radius, true, 1, 0.1);
  ink(255, 128).circle(ball.x, ball.y, ball.radius + 1, false, 1, 0.01);

  // Draw snowball frame...
  if (ballSheet) {
    const rows = 12;
    const tile = ballSheet.width / rows;
    let tx = floor(ball.xang);
    let ty = floor(ball.yang);
    if (tx < 0) tx = rows + tx;
    if (ty < 0) ty = rows + ty;
    tx = rows - 1 - tx;

    paste(
      {
        painting: ballSheet,
        crop: { x: tx * tile, y: ty * tile, w: tile, h: tile },
      },
      ball.x - tile / 2,
      ball.y - tile / 2,
    );
  }

  unpan();

  // 🧮 Data
  ink("yellow").write(`xang: ${num.radians(ball.xang)}`, { x: 6, y: 18 });
  ink("yellow").write(`yang: ${num.radians(ball.yang)}`, { x: 6, y: 18 + 11 });
}

// 🧮 Sim
function sim({ num }) {
  if (!disc) return;

  const step = 0.025;

  // Accelerate the ball as needed.
  if (LEFT) ball.xvel -= step;
  if (RIGHT) ball.xvel += step;
  if (UP) ball.yvel -= step;
  if (DOWN) ball.yvel += step;

  ball.x += ball.xvel;
  ball.y += ball.yvel;

  // Check for any collision with the edges, and slow down the ball if needed.
  if (num.p2.dist(ball, disc) > disc.radius) {
    ball.xvel *= ball.dec / 1.1;
    ball.yvel *= ball.dec / 1.1;
  }

  ball.xang = (ball.xang + ball.xvel) % 360;
  ball.yang = (ball.yang + ball.yvel) % 360;

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
