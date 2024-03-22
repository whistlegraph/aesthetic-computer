// Balls, 2023.8.08.16.57.01
// Balls bouncing on lines.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

const ball = {
  // direction: 
};

// 🥾 Boot
function boot({ screen, geo }) {
  // Runs once at the start.
  ball.circle = new geo.Circle(screen.width / 2, screen.height / 2, 8);
}

// 🧮 Sim
function sim() {

}

// 🎨 Paint
function paint({ wipe, ink, circle }) {
  wipe("purple");
  ink("red").circle(ball.circle.x, ball.circle.y, ball.circle.radius, true);
}

// 🎪 Act
// function act({ event }) {
//  // Respond to user input here.
// }

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
    title: "Balls",
    desc: "Balls bouncing on lines.",
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

export { boot, sim, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
