// Sfx, 2023.6.09.18.41.12
// Testing loading and playing sound effects and samples.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟡] Add startup sound and keyboard sound to keyboard clicks and prompt!
  - [] Eventually add information to read back / get streamed back.
  - [] Check decode delay / trigger a whole keyboard of samples.
  - [] Create a sampling recorder.
  + Done
  - [x] Add these sounds to the remote assets folder.
  - [x] How to load indidivudal sound effects by short name?
  - [x] With a parameter to set the name.
  - [x] Add an interface to play the sample. 
  - [x] Decide an architecture for loading and playing back samples.
#endregion */

// 🥾 Boot
let sfx, btn;
async function boot({ net: { preload }, play, ui, params }) {
  const name = params[0] || "startup";
  console.log("Sound name:", name);
  sfx = await preload(name);
  btn = new ui.TextButton(`Play "${name}"`);
}

// 🖌️ Paint
function paint({ wipe, ink, screen }) {
  wipe(0, 0, 255);
  btn.reposition({ center: "xy", screen });
  btn.paint({ ink });
}

// 🎪 Act
function act({ event: e, sound }) {
  btn.act(e, () => sound.play(sfx));
}

// 📰 Meta
function meta() {
  return {
    title: "Sfx",
    desc: "Testing loading and playing sound effects and samples.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
