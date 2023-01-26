// Video, 23.1.26.12.01 
// Play back and be able to export / upload a recorded video. 

/* #region ✏️ todo
  + Now
  - [] Factor out / modify the old video overlay UI thing to only work
      in this piece.
  - [] Add a "loop" and a "home" (back to title) button.
  - [] Take advantage of the dead time / transcoding time.
  - [] Show a little game or helpful hint. (💡 @alex)
  - [] Only transcode upon tapping export.
  + Done
#endregion */


// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  $.wipe(0, 255, 0);
  $.ink(0).line(0, 0, $.screen.width, $.screen.height)
}

// 🎨 Paint (Executes every display frame)
function paint() {
}

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint }