// Scream, 2023.5.22.21.56.00
// Tell everyone something.

/* #region 📚 README 
  - [] Alert every connected user with a time-synchronized message that
       covers their screen.
#endregion */

/* #region 🏁 TODO 
#endregion */

let server;

// 🥾 Boot
async function boot({ net: { socket }, params }) {
  server = await socket((id, type, content) => {
    if (server.id !== id && type === "scream") {
      console.log("Screamed:", content);
    }
  });

  server.send("scream", "ah!");
}

// 🎨 Paint
// function paint({ ink }) {
//   // Executes every display frame.
// }

// 🎪 Act
// function act({ event }) {
//  // Respond to user input here.
// }

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { boot };

// 📚 Library
//   (Useful functions used throughout the piece)
