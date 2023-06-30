// Scawy Snake, 2024.6.29.00.15.15
// A snake game where you eat colors and grow accordingly.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] 🐍 Make a snake clone with colored lines. 
#endregion */

let snake,
  tail = [],
  fence,
  dir,
  food,
  foodGen,
  clock; // 🧮
const size = 12;

let L, R, U, D; // 🎮
let contact = false; // 🤺

// 🥾 Boot
function boot({
  screen: { width, height, center },
  geo: { Box },
  help: { choose },
  num: { randInt },
}) {
  const gw = floor((width * 0.8) / size),
    gh = floor((height * 0.7) / size); // Grid resolution.
  const fw = size * gw, // Fence width and height.
    fh = size * gh;
  fence = new Box(center.x - fw / 2, center.y - fh / 2, fw, fh);

  //snake = new Box(center.x - size / 2, center.y - size / 2, size); // 🐍
  snake = new Box(fence.x, fence.y, size); // 🐍
  dir = { x: 0, y: 0 }; // 🧭


  foodGen = () => {
    food = {
      color: choose(
        "red",
        "yellow",
        "blue",
        "green",
        "black",
        "white",
        undefined
      ),
      box: new Box(
        fence.x + randInt(gw) * size,
        fence.y + randInt(gh) * size,
        size
      ),
    }; // 🍙
  };

  foodGen();
}

// 🧮 Sim
function sim({ num: { p2 }, gizmo, seconds, geo, pen }) {
  if (L) dir = { x: -1, y: 0 };
  if (R) dir = { x: 1, y: 0 };
  if (U) dir = { x: 0, y: -1 };
  if (D) dir = { x: 0, y: 1 };

  clock ||= new gizmo.Hourglass(seconds(0.1), {
    flipped: (count) => {
      // 🎲 TODO: Use `count` to loop through a temporal decoration
      const next = new geo.Box(p2.add(snake, p2.mul(dir, size)), size);
      if (!fence.contact(next)) return;

      // ❤️‍🔥 TODO: Check direction here and only match the front-facing side!
      contact = next.equal(food?.box); // Check to see if snake is on food.

      if (contact) {
        tail.unshift({ color: food.color, box: geo.Box.from(snake) });
        foodGen();
      }

      // Update each tail piece starting from the one that was just added.
      let start = snake;
      for (let t = 0; t < tail.length; t += 1) {
        const newStart = tail[t].box;
        tail[t].box = start;
        start = newStart;
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
    .box((snake.x + size / 2) + dir.x * s3, (snake.y + size / 2) + dir.y * s3, s3, "center")
    .ink(contact ? "red" : food?.color)
    .box(food?.box);

  tail.forEach((seg) => ink(seg.color).box(seg.box));
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
