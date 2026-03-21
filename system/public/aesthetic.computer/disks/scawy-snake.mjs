// Scawy Snake, 2024.6.29.00.15.15
// A snake game where you eat colors and grow accordingly.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add mobile controls.
    - [] (Could they go outside the square?)
  + [-] Define a constant grid size or "world" that makes sense.
        (Pannable camera?)
  + [] Add start screen / title.
  + [] Add score counter.
  - [] Add game over screen.
  - [] Print game over state onto painting.
  - [] Add leaderboard.
  + Done
  - [x] 🐍 Make a snake (behavior) clone with colored lines. 
#endregion */

let snake,
  tail = [], // Digested pieces. { color, box }
  belly = [], // Pieces that are currently digesting. { color, index }
  fence,
  dir,
  food,
  foodGen,
  clock; // 🧮
const size = 12;
const foodColors = ["red", "yellow", "blue", "green", "black", undefined];

let L, R, U, D; // 🎮

// 🥾 Boot
function boot({
  screen: { width, height, center },
  geo: { Box },
  help: { choose },
  num: { randInt: r },
}) {
  const gw = floor((width * 0.8) / size),
    gh = floor((height * 0.7) / size); // Fence: grid resolution.

  const fw = size * gw, // Fence: pixel width and height.
    fh = size * gh;
  fence = new Box(center.x - fw / 2, center.y - fh / 2, fw, fh); // 🔳
  snake = new Box(
    fence.x + floor(gw / 2) * size,
    fence.y + floor(gh / 2) * size,
    size
  ); // 🐍
  dir = { x: 0, y: 0 }; // 🧭

  foodGen = () => {
    food = {
      color: choose(...foodColors),
      box: new Box(fence.x + r(gw) * size, fence.y + r(gh) * size, size),
    }; // 🍙
  };

  foodGen(); // Generate the first piece of food.
}

// 🧮 Sim
function sim({ num: { p2 }, gizmo, seconds, geo, pen }) {
  if (L) dir = { x: -1, y: 0 };
  if (R) dir = { x: 1, y: 0 };
  if (U) dir = { x: 0, y: -1 };
  if (D) dir = { x: 0, y: 1 };

  clock ||= new gizmo.Hourglass(seconds(0.085), {
    flipped: (count) => {
      // TODO: ^ Use count value for decoration or to step through a rhythm or
      //         melody of some kind. 23.06.30.14.40
      const next = new geo.Box(p2.add(snake, p2.mul(dir, size)), size);
      if (!fence.contact(next)) return;

      // If `snake` is directly on top of `food`.
      if (next.equal(food?.box)) {
        if (tail.length === 0) {
          // Add a new tail piece right away...
          tail.push({ color: food.color, box: geo.Box.from(snake) });
        } else {
          // otherwise,
          belly.push({ color: food.color, index: -1 }); // add a piece to belly.
        }
        foodGen();
      }

      // 🥘 Belly
      // Digest / increment belly pieces.
      for (let b = 0; b < belly.length; b += 1) {
        if (belly[b].index >= tail.length - 1) {
          tail.push({
            color: belly[b].color,
            box: geo.Box.from(tail[tail.length - 1].box),
          });
          belly[b] = null; // Mark for deletion.
        } else {
          belly[b].index += 1;
        }
      }
      belly = belly.filter((item) => item !== null); // Remove digested food.

      // 🪱 Tail: Update each tail piece.
      if (tail.length > 0) {
        let start = geo.Box.from(snake);
        for (let t = 0; t < tail.length; t += 1) {
          const newStart = tail[t].box;
          tail[t].box = start;
          start = newStart;
        }
      }

      snake = next;
    },
    autoFlip: true,
  });

  clock.step();
}

// 🎨 Paint
function paint({ wipe, ink }) {
  const s3 = size / 3;
  wipe(127)
    .ink(110)
    .box(fence, "out")
    .ink("lime")
    .box(fence.grow(size / 2), "out")
    .ink(255)
    .box(snake.x, snake.y, size)
    .ink("lime")
    .box(
      snake.x + size / 2 + dir.x * s3,
      snake.y + size / 2 + dir.y * s3,
      s3,
      "center"
    )
    .ink(food?.color)
    .box(food?.box);

  tail.forEach((seg) => ink(seg.color).box(seg.box));

  belly.forEach((seg) => {
    if (tail[seg.index])
      ink(seg.color)
        .box(tail[seg.index].box)
        .ink()
        .box(tail[seg.index].box, "in");
  });
}

// 🎪 Act
function act({ event: e }) {
  // 🧼 TODO: Short this to... "key:down:left".
  if (e.is("keyboard:down:arrowleft")) L = true;
  if (e.is("keyboard:down:arrowright")) R = true;
  if (e.is("keyboard:down:arrowup")) U = true;
  if (e.is("keyboard:down:arrowdown")) D = true;

  if (e.is("keyboard:up:arrowleft")) L = false;
  if (e.is("keyboard:up:arrowright")) R = false;
  if (e.is("keyboard:up:arrowup")) U = false;
  if (e.is("keyboard:up:arrowdown")) D = false;
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
    title: "Scawy Snake",
    desc: "A snake game where you eat colors and grow accordingly.",
  };
}

export { boot, sim, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
const { abs, floor } = Math;
