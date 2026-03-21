// Song, 2023.7.02.09.27.28
// Notate both a melody and lyrics to sing along to.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  * Slides
  - [] Show whatever current note is closest to the hz value while printing.
  - [⭐] Merge this piece with `song`.
  - [] Add some kind of violin mode.
  - [] Add large "hold" key with "shift" keyboard shortcut.
  - [] How would I capture a tone and then add it to the list of notes in
       my song... / compose.
  * Volume + Pan
  - [] How to vary volume during playback?
  - [] Maybe there needs to be a separate slider or it
        can be for a secondary finger.
  - [] Dragging left and right adjusts the pan.
  + Done
  - [x] Fade the ending note so there is no pop.
  - [x] Make sure reframing works.
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

const mmm = `B F# B F# B F# B D#5 F#5
             E5 C#5 E5 C#5 E5 C#5 A# C#5 F#`.split(/\s+/);

/*
const lyrics = `oh up and down the ci- -i- -ty road
                and in and out the ea- -gle 
                that's the way the mo- -o- -ney goes
                pop! goes the wea- -sel!`.split(/\s+/);
*/

const lyrics = `did u ev- -er meet the kid with star- -ry eyes
                CROSSED OUT EYES! al- -ways thought of life as just
                one big BOX OF LIES! paths to hope- -ful pla- -a- -a- -a- -a- -a- -a- -ces. Bridge- -less like torn pa- -ges.`.split(/\s+/);

const pgtw = `G C5 C5 D5 D5 E5 G5 E5 C5
              G C5 C5 D5 D5 E5 C5
              C5 C5 D5 D5 E5 G5 E5 C5
              A5 D5 F5 E5 C5`.split(/\s+/);

const kid = `E D C E G A B A G F G E E D
             B A G F G E G E G G B B B G
             A F E D E D E D E D E G A F
             G E D C`.split(/\s+/);

let index = 0;
let song = kid;
const m = 24;
let wave = "triangle";

// 🥾 Boot
function boot({ ink, wipe, screen, ui, sound, colon }) {
  wipe(127);
  btn = new ui.Button(m, m, screen.width - m * 2, screen.height - m * 2);
  btn.multitouch = false;
  wave = colon[0] || wave;
  tone = sound.freq(song[index]); // Set starting tone.
}

// 🎨 Paint
function paint({ ink, screen, sound }) {
  // Executes every display frame.

  const note = sound.note(tone);

  btn.paint(() => {
    ink(btn.down ? [245, 220, 50] : [230, 210, 100])
      .box(btn.box)
      .ink(255)
      .write(`${/*song[index]*/ note} - ${tone.toFixed(2)}`, { center: "xy" })
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
function act({
  event: e,
  sound: { synth, freq },
  needsPaint,
  screen,
  num,
  geo,
}) {
  if (e.is("reframed")) {
    btn.box = new geo.Box(m, m, screen.width - m * 2, screen.height - m * 2);
  }

  // Drag up and down to change pitch.
  if (btn.down && e.is("draw")) {
    tone = num.clamp(tone - e.delta.y, 100, 1200);
    sound.update({ tone });
    // console.log("📈 Tone update:", tone);
    needsPaint();
  }

  function push() {
    sound?.kill();
    needsPaint();
    index = (index + 1) % song.length; // Cycle through notes.
    tone = freq(song[index]);
  }

  function down() {
    sound = synth({ type: wave, tone, volume: 1.0, duration: "🔁" });
    needsPaint();
  }

  function cancel() {
    sound?.kill(0.1);
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
