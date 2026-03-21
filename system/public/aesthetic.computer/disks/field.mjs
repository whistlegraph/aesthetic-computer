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

let dragon;

// 🥾 Boot
async function boot() {
  try {
    dragon = await import(`/media/@dreamdealer/piece/dragon.mjs`);
  } catch (err) {
    console.warn("Could not load dragon.");
  }
}

// 🏔️ Background
function background({ wipe }) {
  wipe("olive");
}

// 🎨 Paint
function paint($, world) {
  const { ink, api, savepan, loadpan, pan, unpan } = $;

  ink("darkgreen").box(0, 0, world.width, world.height);
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

  // @dreamdealer/dragon 🐲
  if (dragon) {
    savepan();
    pan(-40, -40);
    pan(256, 256);
    pan(-40, 20);

    ink("green").write("@dreamdealer/dragon", { x: -16, y: -12 });
    const f$ = { ...$ };
    f$.screen = { ...$.screen };
    f$.screen.width = 80;
    f$.screen.height = 80;
    dragon?.paint(f$);
    loadpan();
  }
}

// 🚿 Curtain
function curtain({ ink }) {
  // ink().line();
}

// 🎪 Act
function act() {}

// 🧮 Sim
function sim({ system: { world } }) {
  if (
    world.me &&
    world.size &&
    world.me.moved &&
    world.me.pos.x === world.size.width
  )
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
