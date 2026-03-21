// Bb, 2023.5.31.21.48.45
// A brick breakup game.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Paddle controls. 
  - [] Ball bouncing off all walls and paddle.
  - [] Brick breakup.
  - [] Words?
#endregion */

const brick = { x: null, y: null };
const paddle = { x: null, y: null };
const ball = { x: null, y: null };

// 🥾 Boot
function boot({ screen: { width, height }, num }) {
  brick.x = width / 2;
  brick.y = num.randIntRange(20, 32);
  ball.x = width / 2;
  ball.y = height / 2;
  paddle.x = width / 2;
  paddle.y = height - 20;
}

// 🎨 Paint
function paint({ wipe, ink }) {
  // wipe(0);
  ink(255, 0, 0).box(brick.x, brick.y, 30, 10, "*center");
  ink(0, 255, 0).box(paddle.x, paddle.y, 50, 10, "*center");
  ink(255).box(ball.x, ball.y, 8, "*center");
}

// 🧮 Sim
function sim() {}

// 🎪 Act
function act({ event }) {}

// 🥁 Beat
function beat() {}

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Bb",
    desc: "A brick breakup game.",
  };
}

export { boot, paint, sim, act, beat, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
