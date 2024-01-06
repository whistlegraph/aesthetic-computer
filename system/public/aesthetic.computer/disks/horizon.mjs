// Horizon, 2024.1.06.13.10.52.413
// Walk left or right here.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

const scenery = {
  grasses: [
    { x: 190, y: 170 },
    { x: 276, y: 286 },
    { x: 128, y: 128 },
    { x: 400, y: 400 },
    { x: 500, y: 512 },
  ],
};

// 🥾 Boot
async function boot() {}

// 🏔️ Background
function background({ wipe }) {
  wipe("blue");
}

// 🎨 Paint
function paint({ ink }, world) {
  ink("brown").box(0, 0, world.width, world.height);
  //ink("black").line(0, 0, world.width, world.height);
  //ink("red").line(0, world.height, world.width, 0);

  // Scenery
  scenery.grasses.forEach((grass) => {
    ink("red")
      .line(grass.x, grass.y, grass.x, grass.y - 10)
      .line(grass.x, grass.y, grass.x - 5, grass.y - 6)
      .line(grass.x, grass.y, grass.x + 5, grass.y - 6);
  });
}

// 🚿 Curtain
function curtain({ ink }) {
  // ink().line();
}

// 🎪 Act
function act() {}

// 🧮 Sim
function sim() {}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave() {}

// 📰 Meta
function meta() {
  return {
    title: "Horizon",
    desc: "Walk left or right here.",
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

export const system = "world";
export { boot, background, paint, curtain, act, sim, leave, meta };

// 📚 Library
//   (Useful functions used throughout the piece)