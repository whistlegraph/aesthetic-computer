// Sno, 2023.11.15.12.10.43.464
// A snowball game by @ida, @mxsage and @jeffrey.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟠] Chat / tap to add text above your head.
    - [] See `prompt-system` to implement a TextInput object that can
         bring the keyboard up and back down.
  - [] Put the ball on the server, and instantiate it / simulate it.
    - [] Make a place for the ball to be kicked / simulate the ball on
         the server.
    - [] Implement UDP backend for good gaming transport.
      - [] Basic collision detection.
      - [] Stress test / add global game state.
    - [] Title screen?
  + Done
  - [x] Add in `allPeople_sprite2k.png` and `simPeople_sprite2k.png`.
    - [x] Each animation is 8 frames long.
  - [x] Random character assignment. 
  - [x] Z-sorting.

#endregion */

const { min, floor, abs, acos, sin, cos, sqrt, PI, random, round } = Math;
const { keys } = Object;

let server;
let self;
const lead = { x: 0, y: 0 };
let touchedAt; // For tap-to-jump.
let idleCycles = 0; // For counting idle animations until lie an
let postStandState = "idle";

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

let ballSheet,
  kidSheets = {},
  moundPainting;

const mounds = [
  { x: 0, y: 150, type: "mound" },
  { x: 150, y: 0, type: "mound" },
  { x: -70, y: -80, type: "mound" },
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

  debug = false;
  // Load sprite assets from the server.
  const path = debug ? "/assets/sno" : "https://assets.aesthetic.computer/sno";

  preload(`${path}/allPeople_sprite2k.webp`).then((file) => {
    kidSheets["all"] = file.img;
  });

  preload(`${path}/simPeople_sprite2k.webp`).then((file) => {
    kidSheets["sim"] = file.img;
  });

  // preload(`${path}/spriteWispy2k.png`).then((file) => {
  //   kidSheets["wispy"] = file.img;
  // });

  // preload(`${path}/spriteBunny2k.png`).then((file) => {
  //   kidSheets["bunny"] = file.img;
  // });

  // preload(`${path}/spriteSpike2k.png`).then((file) => {
  //   kidSheets["spike"] = file.img;
  // });

  preload(`${path}/moundFinalSmall.png`).then((file) => {
    moundPainting = file.img;
  });

  // Create a new `kid` with a character type.
  self = kid(
    handle() || "nub",
    help.choose("orange", "yellow", "cyan", "blue", "lime"),
    help.choose("all", "sim"),
    // help.choose("wispy", "bunny", "spike"),
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
      server.send("sno:join", {
        handle: self.handle,
        color: self.color,
        body: self.body,
      });
      console.log("⛄ Welcome:", self.handle, `(${id})`);
      self.id = id;
    }

    if (server.id !== id) {
      if (type === "sno:join") {
        console.log(`sno:join:${id}`, content);
        if (!others[id]) {
          others[id] = kid(content.handle, content.color, content.body);
          server.send("sno:join", {
            handle: self.handle,
            color: self.color,
            body: self.body,
          });
        }
      }
    }
    if (type === "sno:move") {
      if (others[id]) {
        others[id].x = content.x;
        others[id].y = content.y;
        others[id].step = content.step;
        others[id].dir = content.dir;
        others[id].state = content.state;
      }
    }
  });

  ball.rot = quat.setAxisAngle(quat.create(), [1, 0, 0], 0);
}

// 🎨 Paint
function paint({ screen, wipe, ink, pan, unpan, paste, layer }) {
  const short = min(screen.width, screen.height); // Longest view w/ margin.

  wipe(90);

  // pan(cam.x, cam.y);
  // // 🥏 Playground Disc
  disc = { x: 0, y: 0, radius: short / 2.5 };
  // unpan();
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
  function paintMound(mound) {
    if (moundPainting) {
      paste(
        moundPainting,
        mound.x - moundPainting.width / 2,
        mound.y - moundPainting.height / 2 + 16,
        { scale: 1 },
      );
    }
  }

  function paintKid(kid) {
    ink(kid.color).box(kid.x, kid.y - 48, 2, "outline*center");

    if (kidSheets[kid.body]) {
      const tile = kidSheets[kid.body].height / 16;

      const states = {
        walk: 0,
        idle: 1,
        lie: 2,
        sleep: 3,
        stand: 4,
        jump: 5,
        lift: 6,
        throw: 7,
      };

      const tx = floor(kid.step) + states[kid.state] * 8;
      const ty = floor(kid.dir);
      paste(
        {
          painting: kidSheets[kid.body],
          crop: { x: tx * tile, y: ty * tile, w: tile, h: tile },
        },
        kid.x - tile / 2,
        kid.y - tile / 2,
      );
    }
  }

  // 🧑‍🤝‍🧑 Self & Pals
  const all = { ...others };
  all[self.id] = self;
  const allk = keys(all).sort();

  // Collect all mounds and kids into a single list.
  const gameObjects = [...mounds];
  allk.forEach((k) => gameObjects.push(all[k]));

  // TODO: Sort all gameObjects by Y
  gameObjects.sort((a, b) => a.y - b.y);

  gameObjects.forEach((go, i) => {
    if (go.type === "mound") paintMound(go);
    else if (go.type === "kid") paintKid(go);
  });

  // 🦮 Self leed
  ink(self.color).box(lead.x, lead.y - 48, 9, "outline*center");

  unpan();

  // 📏 HUD
  // TODO: Make this a generic module for printing user lists? 23.12.04.15.47
  allk.forEach((k, i) => {
    const row = i * 12;
    ink("black").write(all[k].handle, { x: 7, y: 21 + 1 + row });
    ink(all[k].color).write(all[k].handle, { x: 6, y: 21 + row });
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

  const lastLead = { ...lead };

  if (LEFT) lead.x -= sstep; // Walk self as needed.
  if (RIGHT) lead.x += sstep;
  if (UP) lead.y -= sstep;
  if (DOWN) lead.y += sstep;

  let newSelf = { x: self.x, y: self.y };

  keys(others).forEach((k) => {
    const dir = p2.sub(self, others[k]);
    const dist = Math.sqrt(dir.x * dir.x + dir.y * dir.y);
    const influenceDst = 30;
    const forceMultiplier = 0.1;
    if (dist < influenceDst && dist > 0.1) {
      newSelf.x += forceMultiplier * (influenceDst - dist) * dir.x;
      newSelf.y += forceMultiplier * (influenceDst - dist) * dir.y;
    }
  });

  mounds.forEach((mound) => {
    const dir = p2.sub(self, mound);
    const dist = Math.sqrt(dir.x * dir.x + dir.y * dir.y);
    const influenceDst = 30;
    const forceMultiplier = 0.1;
    if (dist < influenceDst && dist > 0.1) {
      newSelf.x += forceMultiplier * (influenceDst - dist) * dir.x;
      newSelf.y += forceMultiplier * (influenceDst - dist) * dir.y;
    }
  });

  newSelf = { x: self.x, y: self.y };

  if (self.state === "walk" || self.state === "jump") {
    newSelf = {
      x: lerp(newSelf.x, lead.x, 0.015),
      y: lerp(newSelf.y, lead.y, 0.015),
    };
  }

  if (self.state === "walk") {
    const speed = p2.dist(self, newSelf);

    if (speed > 0.01) {
      self.step = (self.step + speed / 7) % 8;
      const movement = {
        x: self.x,
        y: self.y,
        step: self.step,
        dir: self.dir,
        state: self.state,
      };
      if (simCount % 6n === 0n) server.send("sno:move", movement);
    }
  } else if (self.state === "idle") {
    self.step = self.step + 1 / 12;

    if (self.step >= 8) {
      self.step %= 8;
      idleCycles += 1;
    }

    if (idleCycles > 2) {
      idleCycles = 0;
      self.state = "lie";
    }
  } else if (self.state === "jump") {
    self.step = self.step + 1 / 8;
    if (self.step >= 8) state(self, "walk");
  } else if (self.state === "lie") {
    self.step = self.step + 1 / 8;
    if (self.step >= 8) state(self, "sleep");
  } else if (self.state === "sleep") {
    self.step = (self.step + 1 / 16) % 8;
  } else if (self.state === "throw") {
    self.step = self.step + 1 / 12;
    if (self.step >= 8) state(self, "idle");
  } else if (self.state === "stand") {
    self.step = self.step + 1 / 16;
    if (self.step >= 8) {
      self.step = 0;
      state(self, postStandState);
    }
  }

  // Adjust the camera.
  cam.x += self.x - newSelf.x;
  cam.y += self.y - newSelf.y;

  // Update the position.
  self.x = newSelf.x;
  self.y = newSelf.y;

  // Adjust the sprite direction.
  const deg = degrees(Math.atan2(self.x - lead.x, self.y - lead.y));
  let norm = (deg + 180) / 360;
  norm += 0.5 + 0.02;
  norm = norm - floor(norm);

  const angle = floor(norm * 16);
  self.dir = angle;

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
function act({ event: e, screen, num }) {
  // Respond to user input here.
  if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) {
    LEFT = true;
    checkMovementKey();
  }

  if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) {
    LEFT = false;
    checkMovementKey();
  }

  if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) {
    RIGHT = true;
    checkMovementKey();
  }

  if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) {
    RIGHT = false;
    checkMovementKey();
  }

  if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup")) {
    UP = true;
    checkMovementKey();
  }

  if (e.is("keyboard:up:w") || e.is("keyboard:up:arrowup")) {
    UP = false;
    checkMovementKey();
  }

  if (e.is("keyboard:down:s") || e.is("keyboard:down:arrowdown")) {
    DOWN = true;
    checkMovementKey();
  }

  if (e.is("keyboard:up:s") || e.is("keyboard:up:arrowdown")) {
    DOWN = false;
    checkMovementKey();
  }

  if (e.is("keyboard:down:t")) state(self, "throw");

  function checkMovementKey() {
    if (self.state === "jump") return;
    if (!LEFT && !RIGHT && !UP && !DOWN) {
      idle();
    } else {
      state(self, "walk");
    }
  }

  function idle() {
    lead.x = num.lerp(lead.x, self.x, 0.99);
    lead.y = num.lerp(lead.y, self.y, 0.99);
    state(self, "idle");
  }

  // 🛸 Jumping
  if (e.is("keyboard:down:space") && self.state !== "jump") state(self, "jump"); // Spacebar
  if (e.is("touch")) touchedAt = { x: e.x, y: e.y }; // Tap
  if (e.is("lift")) {
    if (num.p2.dist(e, touchedAt) === 0) {
      state(self, "jump");
    } else {
      idle();
    }
  }

  if (e.is("draw")) {
    lead.x += e.delta.x;
    lead.y += e.delta.y;
    state(self, "walk");
  }

  // if (e.is("dropped:bitmap")) {
  // if (e.name.startsWith("sprite")) kidSheet = e.painting;
  // ballSheet = e.painting; // Load a spritesheet.
  // }
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

// `state(self, "lie")`
function state(kid, nextState) {
  if (
    (kid.state === "throw" && nextState === "throw") ||
    (kid.state === "jump" && nextState === "jump")
  ) {
    return;
  }

  if (kid.state === "sleep") {
    postStandState = nextState;
    kid.state = "stand";
  } else {
    kid.state = nextState;
  }

  if (kid.state === "idle" || kid.state === "jump" || kid.state === "throw")
    kid.step = 0;
  idleCycles = 0;
}

function kid(handle, color, body = "wispy") {
  return {
    type: "kid",
    body,
    handle,
    x: 0,
    y: 0,
    state: "idle", // "walk", "idle", "lie", "sleep", "stand", "jump", "lift", "throw"
    step: 0,
    dir: 0,
    color,
  };
}
