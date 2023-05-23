// Scream, 2023.5.22.21.56.00
// Tell everyone something.

/* #region 📚 README 
  - [] Tell the main user they screamed, and return them to the prompt. 
  - [] Ignore the scream for the main user.
  - [] Smartly synchronize that message for all users by looking ahead a bit?
  + Done
  - [x] Alert every connected user with a message that
       covers their screen.
#endregion */

/* #region 🏁 TODO 
#endregion */

let server;

// 🥾 Boot
async function boot({ net: { socket }, params }) {
  server = await socket();
  server.send("scream", params.join(" ") || "Ahh!");
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
