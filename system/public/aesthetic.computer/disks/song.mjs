// Song, 2023.7.02.09.27.28
// Notate both a melody and lyrics to sing along to.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  * Slides
  - [🟡] How to I capture a tone and then add it to the list of notes in
         my song?
  * Volume + Pan
  - [] How to vary volume during playback?
  - [] Maybe there needs to be a separate slider or it
        can be for a secondary finger.
  - [] Dragging left and right adjusts the pan.
  + Done
  - [x] Add big button for starting a continuous sine wave.
  - [x] Pushing the button down starts a tone and letting go ends it.
  - [x] Dragging up and down adjusts the pitch.
  + Later
#endregion */

let btn,
  sound,
  tone = 800;

// 🥾 Boot
function boot({ ink, wipe, screen, ui }) {
  // Runs once at the start.
  wipe(127);

  const m = 24;
  btn = new ui.Button(m, m, screen.width - m * 2, screen.height - m * 2);
}

// 🎨 Paint
function paint({ ink }) {
  // Executes every display frame.
  btn.paint(() =>
    ink(btn.down ? [245, 220, 50] : [230, 210, 100])
      .box(btn.box)
      .ink(255)
      .write(tone, { center: "xy" })
  );
  return false; // Uncomment for an animation loop.
}

// 🎪 Act
function act({ event: e, sound: { synth }, needsPaint, num }) {
  // Drag up and down to change pitch.
  if (btn.down && e.is("draw")) {
    tone = num.clamp(tone - e.delta.y, 200, 1200);
    sound.update({ tone });
    console.log("📈 Tone update:", tone);
    needsPaint();
  }

  btn.act(
    e,
    {
      push: () => {
        sound?.kill();
        needsPaint();
      },
      down: () => {
        sound = synth({
          type: "sine",
          tone: tone,
          volume: 1.0,
          beats: Infinity,
        });
        needsPaint();
      },
      rollover: () => {
        // needsPaint();
      },
      rollout: () => {
        // needsPaint();
      },
      cancel: () => {
        sound?.kill();
        needsPaint();
      },
    }
    // pens?.() // Enables multi-touch support for this button.
  );
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 📰 Meta
function meta() {
  return {
    title: "Song",
    desc: "Notate both a melody and lyrics to sing along to.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)