// List, 2024.1.30.13.18.29.955
// A directory of all system pieces and prompt commands.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Grab list from api. 
#endregion */

let docs;

function boot({ net }) {
  // 📔 Get the docs off the api.
  fetch("https://" + net.host + "/api/docs")
    .then((response) => {
      if (response.status !== 200) {
        throw new Error("Network failure: " + response.status);
      } else return response.json();
    })
    .then((json) => {
      console.log("📚 Got docs:", json);
      docs = json;
    })
    .catch((err) => console.error("🔴 📚 Couldn't get docs:", err));
}

const { keys } = Object;

function paint({ wipe, ink, screen, text }) {
  wipe("black");
  if (!docs) return;
  // TODO: Reference `prutti`
  ink("white").write(
    keys(docs.pieces).join("\n"),
    { x: 6, y: 22 },
    undefined,
    screen.width,
    false,
  );

}

// function act({ event: e }) {
//  // Respond to user input here.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

function meta() {
  return {
    title: "List",
    desc: "A directory of all system pieces and prompt commands.",
  };
}

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
