// Tone, 2023.6.20.18.36.12
// Make a single tone in a specified frequency and wave type.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let sine;

// 🥁 Beat
function beat({ params, sound: { microphone, square, speaker } }) {
  if (!sine) {
    sine = square({
      type: "sine",
      tone: params[0] || 400,
      volume: 1.0,
      beats: Infinity,
    });
  }
}

// 🥾 Boot
function boot({ wipe }) {
  wipe(0, 0, 128);
}

// 🎨 Paint
function paint({ ink }) {
  // Executes every display frame.
  return false;
}

// 📰 Meta
function meta() {
  return {
    title: "Tone",
    desc: "Make a single tone in a specified frequency and wave type.",
  };
}

export { boot, paint, beat, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
