// Song, 2023.7.02.09.27.28
// Notate both a melody and lyrics to sing along to.

//import { noteFrequencies } from "./common/music.mjs";

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  * Slides
  - [🧡] Fade the ending note so there is no pop.
  - [] Add large "hold" key with "shift" keyboard shortcut.
  - [] How would I capture a tone and then add it to the list of notes in
       my song... / compose.
  * Volume + Pan
  - [] How to vary volume during playback?
  - [] Maybe there needs to be a separate slider or it
        can be for a secondary finger.
  - [] Dragging left and right adjusts the pan.
  + Done
  - [x] Add space keyboard shortcut for playback that triggers the button.
        (With no conflicts)
  - [x] Fix multi-touch.
  - [x] Pre-program `pgtw`. 
  - [x] Add big button for starting a continuous sine wave.
  - [x] Pushing the button down starts a tone and letting go ends it.
  - [x] Dragging up and down adjusts the pitch.
  + Later
#endregion */

let btn,
  sound,
  tone = 800;

// G C6 C6 D6 D6 E6 G6 E6 C6
// G -1 C6 C6 D6 D6 E6 G6 E6 C6
// G C6 C D D E G E C

//const pgtw = "F F G G A C6 A F C F F G C6 A".split(" ");
const pgtw = `G C6 C6 D6 D6 E6 G6 E6 C6
              G C6 C6 D6 D6 E6 C6
              C6 C6 D6 D6 E6 G6 E6 C6
              A6 D6 F6 E6 C6`.split(/\s+/);

const lyrics = `oh up and down the ci- -i- -ty road
                and in and out the ea- -gle 
                that's the way the mo- -o- -ney goes
                pop! goes the wea- -sel!`.split(/\s+/);

let index = 0;
let song = pgtw;

// 🥾 Boot
function boot({ ink, wipe, screen, ui, sound }) {
  wipe(127);
  const m = 24;
  btn = new ui.Button(m, m, screen.width - m * 2, screen.height - m * 2);
  btn.multitouch = false;

  tone = sound.freq(song[index]); // Set starting tone.
}

// 🎨 Paint
function paint({ ink, screen }) {
  // Executes every display frame.
  btn.paint(() => {
    ink(btn.down ? [245, 220, 50] : [230, 210, 100])
      .box(btn.box)
      .ink(255)
      .write(`${song[index]} - ${tone.toFixed(2)}`, { center: "xy" })
      .ink("red")
      .write(lyrics[index], { center: "x", y: screen.height / 2 - 16 });

    ink(0).line(0, screen.height - 1, screen.width, screen.height - 1);
    ink(255).line(
      0,
      screen.height - 1,
      ((index + 1) / song.length) * screen.width,
      screen.height - 1,
    );
  });
  return false; // Uncomment for an animation loop.
}

let keyHeld = false;

// 🎪 Act
function act({ event: e, sound: { synth, freq }, needsPaint, num }) {
  // Drag up and down to change pitch.
  if (btn.down && e.is("draw")) {
    tone = num.clamp(tone - e.delta.y, 100, 1200);
    sound.update({ tone });
    // console.log("📈 Tone update:", tone);
    needsPaint();
  }

  function push() {
    sound?.kill();
    // sound?.kill();
    needsPaint();
    index = (index + 1) % song.length; // Cycle through notes.
    tone = freq(song[index]);
  }

  function down() {
    sound = synth({ type: "sine", tone, volume: 1.0, beats: Infinity });
    needsPaint();
  }

  function cancel() {
    sound?.kill();
    needsPaint();
  }

  if (e.is("keyboard:down:space") && !btn.down) {
    btn.down = true;
    keyHeld = true;
    down();
  }

  if (keyHeld && e.is("keyboard:up:space")) {
    btn.down = false;
    keyHeld = false;
    push();
  }

  if (!keyHeld) {
    btn.act(e, {
      push: () => push(),
      down: () => down(),
      cancel: () => cancel(),
    });
  }

  if (e.is("defocus")) cancel();
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