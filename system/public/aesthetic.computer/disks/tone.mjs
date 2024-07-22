// Tone, 2023.6.20.18.36.12
// Make a single tone in a specified frequency and wave type.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Done
  - [x] Make the whole `tone` screen into a single button so it can be
        held down.
#endregion */

let tone, type, sound, d;

function boot({ wipe, slug, params, colon, hud, sound: { synth }, num, help }) {
  wipe(0, 0, 128);
  tone = params[0] || num.randIntRange(50, 4000); // TODO: Make the default random and fill out
  //                                the param on top?
  type = colon[0] || "sine"; // help.choose("sine", "triangle", "square", "sawtooth");

  let label = "notepad";
  if (colon[0]) label = label + ":" + colon[0];
  label += " " + tone;
  hud.label(label);

  synth({ type, tone, volume: 1.0, duration: 0.25, attack: 0.1, decay: 0.99 });
}

function sim({ colon, simCount, jump, leaving, num }) {
  if (colon[0] === "cycle" && !leaving() && simCount >= 1n)
    jump(`tone:cycle~${num.randIntRange(400, 500)}`, true);
}

function paint({ wipe }) {
  // Executes every display frame.
  if (sound) {
    wipe(0, 0, 255 * d)
  } else {
    wipe("darkblue")
  }
}

function act({ event: e, sound: { synth }, num, screen }) {
  if (e.is("touch")) {
    // playing
    d = num.map(num.dist(e.x, e.y, screen.width / 2, screen.height / 2), 0, screen.height / 2, 1, 0);
    sound = synth({ type, tone, volume: d, duration: "🔁", attack: 0.1, decay: 0.99 });
  } else if (e.is("lift")) {
    // stop playing
    sound?.kill(0.25);
    sound = null;
  } else if (e.is('draw') && sound) {
    d = num.map(num.dist(e.x, e.y, screen.width / 2, screen.height / 2), 0, screen.height / 2, 1, 0);
    sound.update({ volume: d });
  }
}

export { boot, sim, paint, act };
