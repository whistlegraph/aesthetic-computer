// Field, 2023.11.30.16.05.21.050
// An open place to walk around.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [x] Move common functionality to a `world.mjs` library file.
    - [-] Move all functions over succesfully. 
    - [] Localize world geometry to this file.
  - [] Store something persistent on the server?
  + Later
  + Done
  - [x] Slow camera following.
  - [x] Add user list support.
  - [x] Finish "world:fields:list".
    - [x] When you join you get the full list,
    - [x] Then when anyone else joins or leaves you get
         the individual joins and leaves.
  - [c] Store persistent position on the server / in the database. 
  - [x] Fix up lerping a bit...
  - [x] Store local position in store.
  - [x] Make the world scrollable with some background grass.
  - [x] Always lerp towards next character positions from the network.
  - [x] Test join `field` simultaneously (with forceProd on) and ensure
        there are no race conditions or conflicts. (Implement jamsocket's locks?)
  - [x] Fix instagram not connecting error.
  - [x] Keyboard stops working after tabbing out and returning. 
    - [x] Android debugger session with @ida's phone.
  - [x] Remove gutter. 
  - [x] Add world bounds.
  - [?] Add enter key hint.
  - [x] Tapping the word in the top left corner should not flash the keyboard. 
  - [x] Get keyboard opening on Mobile Safari.
  - [x] `Escape` key should still go back to the `prompt`.
  - [x] (`) key should still go back to the `prompt`.
  - [x] Add a "special", `smile`, `frown` and `meh` command 😉. 
    - [x] Add color words to change face.
    - [x] Make sure these can be written like: `smile: chat` / using a 
         simple character on the keyboard.
  - [x] Get multi-user networking online. 
  - [x] Add an overhead chat display.
  - [x] Wire up tappable character button to activate the text input.
  - [x] Enter button should close empty prompt.
  - [x] Escape key should close prompt no matter what.
  - [x] Don't snap the cursor all the way back after hitting return
        / keep it at its position.
  - [x] Paste button does not appear when going back to the prompt
        from another piece after entering a single key.
  - [x] `Enter` button appears and disappears at weird times.
#endregion */

// 🥾 Boot
async function boot({ wipe }) {
  wipe(0);
}

// 🎨 Paint
// 😱 TODO: This should just paint the world...
function paint({ wipe }) {
  wipe("olive"); // 🖼️ Backdrop
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

export const system = "world";
export { boot, paint, act, sim, leave, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
