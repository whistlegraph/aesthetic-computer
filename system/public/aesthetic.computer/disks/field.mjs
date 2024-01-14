// Field, 2023.11.30.16.05.21.050
// An open place to walk around.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Future
  - [] Store something persistent on the server.
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
  wipe("olive");
}

// 🎨 Paint
function paint({ ink }, world) {
  ink("green").box(0, 0, world.width, world.height);
  //ink("black").line(0, 0, world.width, world.height);
  //ink("red").line(0, world.height, world.width, 0);

  ink("yellow").line(world.width, 0, world.width, world.height - 1);

  // Scenery
  scenery.grasses.forEach((grass) => {
    ink("lime")
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
function sim({ system: { world } }) {
  if (world.me.moved && world.me.pos.x === world.size.width)
    world.teleport("horizon", { x: 0, y: 10 });
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave() {}

// 📰 Meta
function meta() {
  return {
    title: "Field",
    desc: "An open place to walk around.",
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

export const world = { width: 512, height: 512 };
export const system = "world";
export { boot, background, paint, curtain, act, sim, leave, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
