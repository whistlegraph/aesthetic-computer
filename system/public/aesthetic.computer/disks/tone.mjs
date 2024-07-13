// Tone, 2023.6.20.18.36.12
// Make a single tone in a specified frequency and wave type.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add `cycle` option.
  + Done
  - [x] Fix sticking `tone` on iOS.
#endregion */

// 🧮 Sim
function sim({ colon, simCount, jump, leaving, num }) {
  if (colon[0] === "cycle" && !leaving() && simCount >= 1n)
    jump(`tone:cycle~${num.randIntRange(400, 500)}`, true);
}

// 🥾 Boot
function boot({ wipe, params, sound: { synth } }) {
  wipe(0, 0, 128);
  synth({
    type: "sine",
    tone: params[0] || 400,
    volume: 1.0,
    duration: 0.5,
  });
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

export { boot, paint, meta, sim };

// 📚 Library
//   (Useful functions used throughout the piece)
