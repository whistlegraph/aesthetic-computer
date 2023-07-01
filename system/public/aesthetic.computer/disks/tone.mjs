// Tone, 2023.6.20.18.36.12
// Make a single tone in a specified frequency and wave type.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let sine;

// 🥁 Beat
function beat({ params, sound: { synth } }) {
  // TODO: This should not have to go in `beat`.
  if (!sine) {
    sine = synth({
      type: "sine",
      tone: params[0] || 400,
      volume: 1.0,
      beats: Infinity
    });
  }
}

// 🧮 Sim
function sim({ simCount, jump, num }) {
  if (simCount >= 80n) jump(`tone~${num.randIntRange(700, 1000)}`);
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

export { boot, paint, beat, meta, sim };

// 📚 Library
//   (Useful functions used throughout the piece)
