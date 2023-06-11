// Sfx, 2023.6.09.18.41.12
// Testing loading and playing sound effects and samples.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add an interface to play the sample. 
    - [] With a parameter to set the name.
  - [] Eventually add information to read back / get streamed back.
  - [] Check decode delay / trigger a whole keyboard of samples.
  - [] Create a sampling recorder.
  + Done
  - [-] Decide an architecture for loading and playing back samples.
#endregion */

// 🥾 Boot
let sfx;
async function boot({ net: { preload }, play }) {
  // Runs once at the start.
  sfx = await preload("assets/sounds/AeCo_startup.m4a");
  console.log("SFX", sfx);

  play(sfx); // Try to play regardless of whether there was an audioContext
  //            available.
  // TODO: There should be a way to detect this.
}

// 🖌️ Paint
// function paint({ wipe, noise16Aesthetic }) {
//   // wipe(255, 0, 0);
//   noise16Aesthetic(); // Why does this create a buffer error?
// }

// 🎪 Act
function act({ event: e, play }) {
  if (e.is("touch")) play(sfx);
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
