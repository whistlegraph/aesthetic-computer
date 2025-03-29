// Stample, 2025.1.28.04.11.21.170
// Spread a sample across some pats.

/* 📝 Notes
  - [🩷] Add positional swiping.
  - [] Paint a line from each pen start point to the current point.
  - [] Add loop toggle / switch?
  - [] Add a subtle attack and decay to sample playback. 
  - [] Wait until mouse moves one delta y pixel to determine sample playback
        direction.
  - [] Add ability to shift / scroll start and end points.
  - [] Automatically dip the max volume if multiple samples are playing.
  - [] Add visual printing / stamping of pixel data and loading of that
       data.
  + Done
  - [x] Add `paintSound` to the disk library / make a really good abstraction for
        that.
  - [x] Add live pitch shifting / speed.
  - [x] Show the state of the microphone connection, recording and disconnect
         process.
  - [x] Show a tiny waveform in the record button for live feedback.
  - [x] Record a sample and spread it automatically.
  - [x] Draw the waveform over the buttons.
  - [x] Spread out a predefined sample.
    - [x] Add start and end trimming to sound.play();
 */

const { abs } = Math;

// Layout
const BUTTON_LABEL_CONNECTING = "Wait...";
const menuHeight = 32;
const labelHeight = 24;
const maxPats = 10;

let loop = true; // Global setting.

// System
let sfx,
  btns = [],
  pats = 1,
  anyDown = false,
  sampleId,
  sampleData,
  mic,
  micRecordButton,
  micRecordButtonLabel = "Connect",
  micConnected = false,
  patsButton;

const sounds = [],
  progressions = [];

const keyToSfx = { 1: 0, 2: 1, 3: 2, 4: 3, 5: 4, 6: 5, 7: 6, 8: 7, 9: 8, 0: 9 };
const sfxToKey = Object.fromEntries(
  Object.entries(keyToSfx).map(([key, index]) => [index, Number(key)]),
);

const { floor } = Math;

async function boot({
  net: { preload },
  sound: { microphone, getSampleData, enabled },
  play,
  ui,
  params,
  screen,
  delay,
}) {
  // const name = params[0] || "startup";
  const name = "startup"; // TODO: Recall previous samples from `store`.
  if (params[0]) pats = parseInt(params[0]);
  sampleId = await preload(name);
  genPats({ screen, ui });
  micRecordButton = new ui.Button(0, screen.height - 31, 64, 31);
  mic = microphone; // Microphone access.

  patsButton = new ui.Button(screen.width - 24, 0, 24, labelHeight - 1);

  if (mic.permission === "granted" && enabled()) {
    // TODO: Also check to see if we have a working audioContext yet here...
    micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
    delay(() => {
      microphone.connect();
    }, 15);
  }

  getSampleData(sampleId).then((data) => {
    sampleData = data;
    // console.log("🔴 Sample Data:", sampleData);
  });
}

function sim({ sound }) {
  sounds.forEach((sound, index) => {
    // Get progress data.
    sound?.progress().then((p) => (progressions[index] = p.progress));
  });

  mic?.poll(); // Query for updated amplitude and waveform data.
  sound.speaker?.poll();
}

function paint({ api, wipe, ink, sound, screen, num, text, help, pens }) {
  if (mic.recording) {
    wipe("red");
  } else {
    wipe(0, 0, 255);
  }
  btns.forEach((btn, index) => {
    btn.paint(() => {
      ink(btn.down ? "white" : "cyan").box(btn.box); // Paint box a teal color.
      ink("black").box(btn.box, "out"); // Outline in black.
      // const prog = (1 - sounds[index].from)
      // console.log("need from:", sounds[index].options.from);
      // const prog = sounds[index]?.options.from || 0; // / progressions[index];

      let prog = 0;
      if (sounds[index]?.options.speed < 0) {
        prog = sounds[index].options.from;
      } else if (sounds[index]?.options.speed > 0) {
        prog = sounds[index].options.from;
      }

      let options = sounds[index]?.options;

      if (options) {
        // console.log(
        //   "From:",
        //   options.from,
        //   "To:",
        //   options.to,
        //   "Speed:",
        //   sounds[index]?.options.speed,
        // );

        const space = prog * btn.box.h;
        const negative = btn.box.h - space;
        let startY, height;

        if (options.speed > 0 || !options.speed) {
          // startY = btn.box.y;
          // console.log(options.to, options.from);
          // startY = btn.box.y + (1 - options.to) * btn.box.h;
          height = (1 - options.from) * btn.box.h;
          height = btn.box.h;
          startY = btn.box.y;
        } else {
          startY = btn.box.y + (1 - options.to) * btn.box.h;
          height = options.to * btn.box.h;
        }

        // console.log(
        //   "StartY",
        //   startY,
        //   "Height",
        //   height,
        //   "From:",
        //   options.from,
        //   "To:",
        //   options.to,
        // );

        if (progressions[index]) {
          ink("magenta").line(
            0,
            startY /* + 2*/,
            screen.width,
            startY /* + 2*/,
          );
          // console.log(startY);

          // ink("green", 64).box(
          //   btn.box.x,
          //   startY, // btn.box.y + btn.box.h,
          //   btn.box.w,
          //   height,
          //   // -btn.box.h * prog - progressions[index] * negative, // progressions[index],
          // );

          let y;
          let basey;
          const originaly = 24;
          if (options.speed > 0 || !options.speed) {
            basey = floor(
              originaly + (1 - options.from) * (btn.box.h * btns.length),
            ); // btn.box.y + (1 - options.from) * btn.box.h;

            y =
              btn.box.y +
              /* (1 - options.from) */ 1 *
                (1 - progressions[index]) *
                btn.box.h;

            // console.log(basey);
            //y =
            //  btn.box.y +
            //  (1 - options.from / (1 - progressions[index])) * btn.box.h;
          } else {
            basey = btn.box.y + (1 - options.to) * btn.box.h;
            y = btn.box.y + (1 - options.to * progressions[index]) * btn.box.h;
          }

          ink("orange").line(0, y, btn.box.x + btn.box.w, y);
          ink("blue").line(0, basey, btn.box.x + btn.box.w, basey);
          // ink("lime").line(0, 100, btn.box.x + btn.box.w, 100);

          // const y =
          // btn.box.y + btn.box.h * (1 - prog) - progressions[index] * negative;
          // const y = btn.box.y + btn.box.h * (1 - progressions[index]);
          // ink("red").line(0, y, btn.box.x + btn.box.w, y);
        }
      }

      ink("black").write(
        sfxToKey[btns.length - 1 - index],
        btn.box.x + 4,
        btn.box.y + 4,
      );
    });

    micRecordButton.paint((btn) => {
      const color = mic.connected ? "red" : "orange";
      //if (mic.connected)

      ink(btn.down ? "white" : color).box(btn.box);
      ink(btn.down ? color : "white").box(btn.box, "inline");

      ink(btn.down ? color : "white").write(
        micRecordButtonLabel,
        btn.box.x + btn.box.w / 2 - text.width(micRecordButtonLabel) / 2,
        btn.box.y + btn.box.h / 2 - text.height(micRecordButtonLabel) / 2,
      );

      // Graph microphone (1 channel)
      if (mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
        sound.paint.waveform(
          api,
          mic.amplitude,
          mic.waveform,
          btn.box.x,
          btn.box.y,
          btn.box.w - 1,
          btn.box.h,
        );
      }
    });
  });

  patsButton.paint((btn) => {
    ink("yellow", btn.down ? 128 : 64).box(btn.box);
  });

  ink("white").write(pats, { right: pats > 9 ? 6 : 8, top: 6 });

  // console.log(sound.speaker.amplitudes.left);

  const availableWidth = patsButton.box.x - 54;

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 16),
    54,
    0,
    availableWidth,
    24 - 2,
    [255, 0, 0, 255],
  );

  if (sampleData) {
    sound.paint.waveform(
      api,
      num.arrMax(sampleData),
      num.arrCompress(sampleData, 256), // 🔴 TODO: This could be made much faster.
      0,
      labelHeight,
      screen.width,
      screen.height - menuHeight - labelHeight,
      [0, 0, 255, 32],
      { direction: "bottom-to-top" },
    );
  }

  if (pens()) {
    pens().forEach((p) => {
      if (p.dragBox) {
        ink().line(
          p.dragBox.x,
          p.dragBox.y,
          p.dragBox.x + (p.dragBox.w || 0),
          p.dragBox.y + (p.dragBox.h || 0),
        );
      }
    });
  }
}

const btnSounds = {};

function act({ event: e, sound, pens, screen, ui, notice, beep }) {
  const sliceLength = 1 / btns.length; // Divide the total duration (1.0) by the number of buttons.

  btns.forEach((btn, index) => {
    let from = (btns.length - 1 - index) * sliceLength;
    let to = from + sliceLength;
    btn.act(
      e,
      {
        down: (btn, opts) => {
          // if (downs[note]) return false; // Cancel the down if the key is held.
          anyDown = true;
          if (btn.down) return false;

          // if (opts?.keyboard) {
          // const fromPos = from; // 1 - (/*e.y -*/ 0 -  btn.box.y) / btn.box.h;
          // from = fromPos;
          sounds[index] = sound.play(sampleId, { from, to, loop: true });
          // }

          // const fromPos = 1 - (e.y - btn.box.y) / btn.box.h;
          // from = fromPos;
          // sounds[index] = sound.play(sampleId, { from, to, loop });

          // TODO: Figure out a cool attack and decay on these.
          // console.log("Playing sound index:", index);
          if (sound.microphone.connected) sound.microphone.disconnect();
        },

        over: (btn) => {
          // if (btn.up && anyDown) {
          //  btn.up = false;
          //  btn.actions.down(btn);
          // }
          // console.log("over");
        },
        out: (btn) => {
          // btn.down = false;
          // btn.actions.up(btn);
        },
        up: (btn, opts) => {
          // if (downs[note]) return false;
          //if (btn.box.contains(e.dragBox)) {
          //if (
          //  btn.downPointer === 0 ||
          //  (e.pointer === btn.downPointer && btn.box.contains(e))
          //) {

          if (e.pointer === btn.downPointer || opts?.keyboard) {
            sounds[index]?.kill(0.1);
            delete btnSounds[index];
            return true;
          } else {
            return false;
          }

          // return true;
          //} else {
          // return e.pointer !== btn.downPointer;
          //}
          //}
          // console.log("Killing sound index:", index, sounds[index]);
        },
        scrub: (btn) => {
          /*
          if (abs(e.delta.y) > 0 && !btnSounds[index]) {
            // console.log(`Scrub ${index}:`, e.delta);
            // sounds[index] = sound.play(sampleId, { from, to });
            const fromPos = 1 - (e.y - btn.box.y) / btn.box.h;
            from = fromPos;

            let speed = 1;

            if (e.delta.y > 0) {
              // to = 0;
              let tmpFrom = from;
              from = 0;
              to = tmpFrom;
              speed = -1;
            }

            // TODO: How do all these parameters relate?
            // console.log("📗 From:", from, "To:", to);

            btnSounds[index] = true;
            sounds[index] = sound.play(sampleId, { from, to, speed, loop });
          }
          */

          // if (e.pointer === btn.downPointer) {
          if (abs(e.delta.y) > 0) {
            // console.log(`Pitch shift ${index}:`, e.delta.x);
            sounds[index]?.update({ shift: 0.03 * -e.delta.y });
            // sound.play(startupSfx, { pitch: freq(tone) });
          }
          // }
        },
      },
      pens?.(),
    );
  });

  micRecordButton?.act(e, {
    down: () => {
      if (!sound.microphone.connected) {
        sound.microphone.connect();
        micRecordButtonLabel = BUTTON_LABEL_CONNECTING;
      } else {
        setTimeout(() => {
          sound.microphone.rec(); // Start recording.
        }, 50);
      }
    },
    up: async (btn) => {
      if (sound.microphone.recording) {
        const { id, data } = await sound.microphone.cut(); // Get the sample.
        sampleData = data;
        sampleId = id;
        console.log("🎤 Microphone sample id:", sampleId);
      }
    },
  });

  patsButton?.act(e, () => {
    pats += 1;
    beep();
    if (pats > maxPats) pats = 1;
    genPats({ screen, ui });
  });

  if (e.is("microphone-connect:failure")) {
    console.log("🎤 Failed mic connection:", e);
    if (e.reason) notice(e.reason.toUpperCase(), ["yellow", "red"]);
    micRecordButtonLabel = "Connect";
  }

  if (e.is("microphone-connect:success")) {
    console.log("🎤 Connected.");
    micRecordButtonLabel = "Record";
  }

  if (e.is("keyboard:down") || e.is("keyboard:up")) {
    const index = keyToSfx[e.key];
    if (index !== undefined && btns[btns.length - 1 - index]) {
      // Ensure index is valid and button exists
      const btn = btns[btns.length - 1 - index];
      if (e.is("keyboard:down") && !btn.down) {
        // console.log(`${e.key} key pressed!`);
        btn.actions.down(btn, { keyboard: true });
        btn.downPointer = 0;
        btn.down = true;
      } else if (e.is("keyboard:up")) {
        btn.down = false;
        btn.actions.up(btn, { keyboard: true });
        btn.downPointer = undefined;
      }
    }
  }

  if (e.is("keyboard:down:arrowdown")) {
    pats -= 1;
    if (pats < 1) pats = maxPats;
    beep();
    genPats({ screen, ui });
  }

  if (e.is("keyboard:down:arrowup")) {
    pats += 1;
    if (pats > maxPats) pats = 1;
    beep();
    genPats({ screen, ui });
  }

  if (e.is("reframed")) {
    genPats({ screen, ui });
    // micRecordButton.reposition()
    micRecordButton.box.y = screen.height - 32; // = new ui.Button(0, screen.height - 32, 32, 32);
    patsButton.box.x = screen.width - patsButton.box.w; // = new ui.Button(0, screen.height - 32, 32, 32);
  }
}

export { boot, paint, act, sim };

// 📚 Library

// Generate sectional strips of buttons to split the sample by.
function genPats({ screen, ui }) {
  btns.length = 0;
  for (let i = 0; i < pats; i += 1) {
    const strip = (screen.height - menuHeight - labelHeight) / pats,
      x = 0,
      y = labelHeight + strip * i,
      width = screen.width,
      height = strip;
    btns.push(new ui.Button(x, y, width, height));
  }
}
