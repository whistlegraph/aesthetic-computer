// Rain, 2023.12.21.17.00.56.185
// Rain is simply falling on someone, drawn by Aspen.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add the song notes for "rain, rain, go away".
  + Done
  - [x] Add Aspen's graphic.
  - [x] Add rain.
#endregion */

let drops = [],
  guy;

// 🥾 Boot
function boot({ net: { preload }, debug }) {
  // Runs once at the start.
  const path = debug
    ? "/assets/rain"
    : "https://assets.aesthetic.computer/rain";
  preload(`${path}/aspens-rain-guy-nobg.png`).then(({ img }) => (guy = img));
}

// 🧮 Sim
function sim({ simCount, screen, num, help: { repeat, choose } }) {
  // Generate drops.
  //if (simCount % 16n === 0n) {
  repeat(num.randIntRange(1, 16), () => {
    drops.push({
      x: num.randInt(screen.width),
      y: 0,
      speed: num.randIntRange(5, 10),
      state: "alive",
      color: choose("blue", "teal"),
    });
  });
  //}

  drops.forEach((drop) => {
    drop.y += drop.speed * 0.35; // Make drops fall.
    if (drop.y >= screen.height) drop.state = "dead";
  });

  drops = drops.filter((drop) => drop.state !== "dead"); // Remove dead drops.
}

// 🎨 Paint
function paint({ wipe, ink, screen, paste }) {
  wipe("darkblue"); // Clear's the screen. Can use R, G, B or CSS colors.
  ink("green").box(0, screen.height - 20, screen.width);
  paste(guy, screen.width / 2 - guy.width / 2, screen.height - guy.height);
  drops.forEach((drop) => ink(drop.color).point(drop));
}

// 🎪 Act
// function act({ event: e }) {
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
    title: "Rain",
    desc: "Rain is simply falling on someone.",
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
