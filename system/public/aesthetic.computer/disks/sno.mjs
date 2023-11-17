// Sno, 2023.11.15.12.10.43.464
// A snowball game by @ida, @mxsage and @jeffrey.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Random character assignment. 
  - [] Much better multiplayer.
#endregion */

const { min, floor, abs, acos, sin, cos, sqrt, PI, random, round } = Math;
const { keys } = Object;

let server;
let self;
const lead = { x: 0, y: 0 };

let LEFT,
  RIGHT,
  UP,
  DOWN = false;

let others = {};

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
  points: [],
};

let ballSheet, kidSheet, moundPainting;

const mounds = [
  { x: 0, y: 0 },
  { x: 150, y: 0 },
  { x: -70, y: -80 },
];

let BLEFT,
  BRIGHT,
  BUP,
  BDOWN = false;

let cam, disc;

// 🥾 Boot
function boot({
  num: { quat },
  help,
  net: { socket, preload },
  handle,
  debug,
  screen,
}) {
  cam = { x: screen.width / 2, y: screen.height / 2, scale: 1 };

  // Load sprite assets from the server.
  const path = debug ? "/assets/sno" : "https://assets.aesthetic.computer/sno";
  // preload(`${path}/sprite1k.png`).then((file) => {
  // preload(`${path}/spriteWispy1k.png`).then((file) => {
  // preload(`${path}/spriteSpike2k.png`).then((file) => {
  preload(`${path}/spriteBunny2k.png`).then((file) => {
    // preload(`${path}/spriteWispy2k.png`).then((file) => {
    kidSheet = file.img;
  });

  preload(`${path}/moundFinalSmall.png`).then((file) => {
    moundPainting = file.img;
  });

  self = kid(
    handle() || "nub",
    help.choose("orange", "yellow", "cyan", "blue", "lime"),
  );

  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      delete others[id];
    }

    if (type === "joined") {
      console.log("️👋 Hello:", id);
    }

    if (type.startsWith("connected")) {
      server.send("sno:join", { handle: self.handle, color: self.color });
      console.log("⛄ Welcome:", self.handle, `(${id})`);
      self.id = id;
    }

    if (server.id !== id) {
      if (type === "sno:join") {
        console.log(`sno:join:${id}`, content);
        if (!others[id]) {
          others[id] = kid(content.handle, content.color);
          server.send("sno:join", { handle: self.handle, color: self.color });
        }
      }
    }
    if (type === "sno:move") {
      if (others[id]) {
        others[id].x = content.x;
        others[id].y = content.y;
        others[id].step = content.step;
        others[id].dir = content.dir;
      }
    }
  });

  ball.rot = quat.setAxisAngle(quat.create(), [1, 0, 0], 0);
}

// 🎨 Paint
function paint({ screen, wipe, ink, pan, unpan, paste, layer }) {
  const short = min(screen.width, screen.height); // Longest view w/ margin.

  wipe(90);

  // Horizon
  // ink(40).box(0, screen.height / 8, screen.width, screen.height);
  // ink(25).box(0, screen.height / 4, screen.width, screen.height);
  // ink(0).box(0, 0, screen.width, screen.height / 8);

  pan(cam.x, cam.y);

  // 🥏 Playground Disc
  disc = { x: 0, y: 0, radius: short / 2.5 };
  // wipe(64)
  // ink(255, 5) // Snow disc filled,
  // .circle(disc.x, disc.y, cam.scale * disc.radius, true, 1, 1)
  // .ink(255, 8) // w/ outline.
  // .circle(disc.x, disc.y, cam.scale * disc.radius, false, 1, 1);

  unpan();
  layer(1);
  pan(cam.x, cam.y);

  // ⚾ Snowball
  const b = ball;
  b.radius = disc.radius / 8;
  ink(127, 64).circle(b.x, b.y, b.radius, true, 1, 0.1);
  ink(127, 128).circle(b.x, b.y, b.radius + 1, false, 1, 0.01);

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

  if (b.points?.length > 0) {
    b.points.forEach(({ point: p, color: c }) => {
      ink(200, 127 - 127 * p[2]).box(
        b.x + p[0] * b.radius,
        b.y + p[1] * b.radius,
        1,
        "center",
      );
    });
  }

  unpan();
  layer(0);
  pan(cam.x, cam.y);

  // 🏔️ Mounds
  if (moundPainting) {
    mounds.forEach((mound) => {
      paste(
        moundPainting,
        mound.x - moundPainting.width / 2,
        mound.y - moundPainting.height / 2,
        { scale: 1 },
      );
    });
  }

  // 🧑‍🤝‍🧑 Self & Pals
  const all = { ...others };
  all[self.id] = self;
  const allk = keys(all).sort();

  allk.forEach((k, i) => {
    const kid = all[k];
    ink(kid.color).box(kid.x, kid.y - 48, 2, "outline*center");

    if (kidSheet) {
      const tile = kidSheet.height / 16;
      const tx = floor(kid.step);
      const ty = floor(kid.dir);
      paste(
        {
          painting: kidSheet,
          crop: { x: tx * tile, y: ty * tile, w: tile, h: tile },
        },
        kid.x - tile / 2,
        kid.y - tile / 2,
      );
    }
  });

  // 🦮 Self leed
  ink(self.color).box(lead.x, lead.y - 48, 9, "outline*center");

  unpan();

  // 📏 HUD
  allk.forEach((k, i) => {
    const row = i * 10;
    ink("black").write(all[k].handle, { x: 7, y: 18 + 1 + row });
    ink(all[k].color).write(all[k].handle, { x: 6, y: 18 + row });
  });

  // 🧮 Data
  // ink("yellow").write(`xang: ${num.radians(b.xang)}`, { x: 6, y: 18 });
  // ink("yellow").write(`yang: ${num.radians(b.yang)}`, { x: 6, y: 18 + 11 });
}

// 🧮 Sim
function sim({
  num: { p2, vec3, vec4, quat, mat4, lerp, degrees, map },
  simCount,
}) {
  if (!disc) return;

  // 🚶 Walking...
  const sstep = 2;

  if (LEFT) lead.x -= sstep; // Walk self as needed.
  if (RIGHT) lead.x += sstep;
  if (UP) lead.y -= sstep;
  if (DOWN) lead.y += sstep;

  const newSelf = {
    x: lerp(self.x, lead.x, 0.015),
    y: lerp(self.y, lead.y, 0.015),
  };

  const speed = p2.dist(self, newSelf);

  if (speed > 0.01) {
    self.step = (self.step + speed / 7) % 8;
    const movement = { x: self.x, y: self.y, step: self.step, dir: self.dir };
    if (simCount % 6n === 0n) server.send("sno:move", movement);
  }

  cam.x += self.x - newSelf.x;
  cam.y += self.y - newSelf.y;

  self.x = newSelf.x;
  self.y = newSelf.y;

  const deg = degrees(Math.atan2(self.x - lead.x, self.y - lead.y));
  let norm = (deg + 180) / 360;
  norm += 0.5 + 0.02;
  norm = norm - floor(norm);

  const angle = floor(norm * 16);
  self.dir = angle;

  // Camera follows self...
  // cam.x = lerp(cam.x, self.x - cam.x, 0.01);
  // cam.y = lerp(cam.y, self.y - cam.y, 0.01);

  // ⚾ Ball
  const bstep = 0.015;

  // Randomly adjust the ball force.
  if (random() > 0.99) BLEFT = !BLEFT;
  if (random() > 0.99) BRIGHT = !BRIGHT;
  if (random() > 0.99) BUP = !BUP;
  if (random() > 0.99) BDOWN = !BDOWN;

  if (BLEFT) ball.xvel -= bstep; // Accelerate the ball as needed.
  if (BRIGHT) ball.xvel += bstep;
  if (BUP) ball.yvel -= bstep;
  if (BDOWN) ball.yvel += bstep;

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

  const dist = bstep * 2 * p2.dist(ball, { x: ball.px, y: ball.py });

  // Set ball axis.
  if (dist > 0.0001) {
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

    const permutations = [
      // [0, 0, 1, 1],
      // [0, 0.5, 0, 1],
      // [1, 0, 0, 1],
      // [-1, 0, 0, 1],
      // [0, -1, 0, 1],
      // [0, 0, -1, -1],
    ];

    const numPoints = 100; // Number of points you want to add
    const radius = 1; // Assuming a unit sphere for simplicity

    // Generate points in spherical coordinates and convert them to Cartesian coordinates
    for (let i = 0; i < numPoints; i++) {
      const phi = acos(1 - 2 * (i / numPoints)); // Polar angle
      const theta = PI * (3 - sqrt(5)) * i; // Azimuthal angle (Golden Angle)

      const x = radius * sin(phi) * cos(theta);
      const y = radius * sin(phi) * sin(theta);
      const z = radius * cos(phi);

      // Add the new point to the permutations array
      permutations.push([x, y, z, 1]); // Assuming w-component as 1
    }

    const colors = [
      [255, 0, 0],
      [0, 255, 0],
      [0, 0, 255],
      [255, 255, 0],
      [0, 255, 255],
      [255, 0, 255],
      [255, 255, 255],
    ];

    const results = [];

    for (let i = 0; i < permutations.length; i++) {
      const p = permutations[i];
      const result = vec4.transformMat4(
        vec4.create(),
        vec4.fromValues(p[0], p[1], p[2], p[3]),
        m4,
      );
      results.push({ point: result, color: colors[i % colors.length] });
    }

    ball.points = results;
  } else {
    ball.axis = undefined;
  }

  ball.xvel *= ball.dec; // Apply decceleration no matter what.
  ball.yvel *= ball.dec;
}

// 🎪 Act
function act({ event: e, screen }) {
  // Respond to user input here.
  if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) LEFT = true;
  if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) LEFT = false;

  if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) RIGHT = true;
  if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) RIGHT = false;

  if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup")) UP = true;
  if (e.is("keyboard:up:w") || e.is("keyboard:up:arrowup")) UP = false;

  if (e.is("keyboard:down:s") || e.is("keyboard:down:arrowdown")) DOWN = true;
  if (e.is("keyboard:up:s") || e.is("keyboard:up:arrowdown")) DOWN = false;

  if (e.is("draw")) {
    lead.x += e.delta.x;
    lead.y += e.delta.y;
  }

  if (e.is("dropped:bitmap")) {
    if (e.name.startsWith("sprite")) kidSheet = e.painting;
    // ballSheet = e.painting; // Load a spritesheet.
  }
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

function kid(handle, color) {
  return { handle, x: 0, y: 0, state: "still", step: 0, dir: 0, color };
}
