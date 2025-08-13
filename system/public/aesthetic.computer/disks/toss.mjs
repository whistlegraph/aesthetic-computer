// Toss, 2024.11.24.01.04.50.399
// Play two oscillators at once.

/* 📝 Notes
  - [] the corner label dragging is broken / needs an exception in ui for the scrub event/
  - [] Add a horizontal line for low -> hi pitch.
  - [] Make a `safe` tap area to prevent scrolling or just require at least 2-3px deadzone before
       scrubbing is toggled, which can reset on lift.
  - [] Do full volume at center of strip and less on the edges.
  - [] Consider a bounce or inertia mechanic that works like a bouncing scroll.
  - [] Change sound type per strip.
  - [] Add ability to record a sample into a strip.
  - [] Map notes to strips or load certain presets.
  - [] Maybe rattle could be done with touch in addition to shaking the phone?
  + Done
  - [x] Add keyboard shortcuts for the strips.
  - [x] Add auto-resizing to buttons.
  - [x] Add attack and decay similar to `notepat`.
  - [x] Make it so there can be N strips and they can be layed out either
       "along" or "across" like `toss across` and have a number like `toss across 8` for 8 strips where dragging
       left and right changes the freq.
  - [x] Add vertical separator for the two strips.
*/

let bands = [];
let type;
const startTone = 220;
const attack = 0.005;
const killFade = 0.05;

// First character is for upping the pitch, second is for triggering, third
// is for downing.
const shortcuts = [
  "1qa",
  "2ws",
  "3ed",
  "4rf",
  "5tg",
  "6yh",
  "7uj",
  "8ik",
  "9ol",
  "0p;",
  "-['",
  "=]\\",
];

const theme = [
  "red",
  "orange",
  "green",
  "blue",
  "brown",
  "gray",
  "aqua",
  "purple",
  "pink",
  "rose",
  "violet",
  "lilac",
];

const { min, max } = Math;

const toneLow = 5;
const toneHigh = 1600 * 2;

function makeBands({ api, colon }, count) {
  bandCount = count;
  type = colon[0] || "sine";
  for (let i = 0; i < count; i += 1) {
    bands[i] = {
      sound: undefined,
      tone: startTone * (i / 2 + 1),
    };
  }
  layoutBandButtons(api);
}

function layoutBandButtons({ screen, ui }) {
  const hw = screen.width / 2;
  const segWidth = screen.width / bandCount;
  for (let i = 0; i < bandCount; i += 1) {
    const button = new ui.Button(i * segWidth, 0, segWidth, screen.height);
    button.offScreenScrubbing = true; // Enable off-screen scrubbing for toss buttons
    button.id = `toss-band-${i}`;  // Add identifier for debugging
    bands[i].btn = button;
  }
}

let bandCount;

function boot({ api, params }) {
  makeBands(api, min(shortcuts.length, parseInt(params[0]) || 2));
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write, num }) {
  wipe("purple"); // Clear the background.
  bands.forEach((band, index) => {
    const btn = band.btn;
    btn.paint((b) => {
      const hue = num.map(band.tone, toneLow, toneHigh, 0, 359.9);
      const colorA = num.hslToRgb(hue, 100, 30); // theme[index];
      const colorB = num.hslToRgb(hue, 50, 10); // theme[index];
      ink(b.down ? colorA : colorB).box(b.box.x, b.box.y, b.box.w, b.box.h);
      if (b.box.x !== 0) {
        ink("white", 128).line(b.box.x, b.box.y, b.box.x, b.box.y + b.box.h);
      }
      const toneHeight = num.map(band.tone, toneLow, toneHigh, screen.height, 0);
      ink("cyan", 64).line(b.box.x, toneHeight, b.box.x + b.box.w, toneHeight);
      ink("white");
      write(band.tone, {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 - 8,
      });
      ink("yellow");
      write(shortcuts[index][1], {
        center: "x",
        x: b.box.x + b.box.w / 2 + 2,
        y: b.box.h / 2 + 32,
        size: 2,
      });

      ink("green");
      write("+ " + shortcuts[index][0], {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 - 22,
      });

      ink("red");
      write("- " + shortcuts[index][2], {
        center: "x",
        x: b.box.x + b.box.w / 2,
        y: b.box.h / 2 + 6,
      });
    });
  });
  const gap = 32;
}

function act({ event: e, api, sound, pens }) {
  // Resize
  if (e.is("reframed")) layoutBandButtons(api);

  // Pointer input.
  bands.forEach((band, index) => {
    const btn = band.btn;
    const left = index === 0;

    btn.act(
      e,
      {
        down: () => {
          band.sound = sound.synth({
            type,
            attack,
            tone: band.tone,
            duration: "🔁",
          });
        },
        up: () => {
          band.sound?.kill(killFade);
          band.sound = null;
        },
        cancel: () => {
          // Handle button cancellation (including edge detection)
          band.sound?.kill(killFade);
          band.sound = null;
        },
        over: (btn) => {
          // Simple rollover - no complex activation logic
        },
        out: (btn) => {
          // Simple rollout - no complex deactivation logic
        },
        scrub: (g) => {
          band.tone -= e.delta.y;

          if (band.tone < toneLow) band.tone = toneLow;
          if (band.tone > toneHigh) band.tone = toneHigh;

          band.sound.update({
            tone: band.tone,
            duration: 0.05,
          });
        },
      },
      pens?.(),
    );
  });

  // Keyboard shortcuts.
  bands.forEach((band, index) => {
    if (index >= shortcuts.length) return;
    const [up, play, down] = shortcuts[index];

    if (e.is(`keyboard:down:${up}`)) band.upping = true;
    if (e.is(`keyboard:up:${up}`)) band.upping = false;
    if (e.is(`keyboard:down:${down}`)) band.downing = true;
    if (e.is(`keyboard:up:${down}`)) band.downing = false;

    if (e.is(`keyboard:down:arrowdown`) && band.sound) band.downing = true;
    if (e.is(`keyboard:up:arrowdown`) && band.sound) band.downing = false;

    if (e.is(`keyboard:down:arrowup`) && band.sound) band.upping = true;
    if (e.is(`keyboard:up:arrowup`) && band.sound) band.upping = false;

    if (e.is(`keyboard:down:${play}`)) {
      if (!band.btn.down) {
        band.btn.down = true;
        band.sound = sound.synth({
          type,
          attack,
          tone: band.tone,
          duration: "🔁",
        });
      }
    }

    if (e.is(`keyboard:up:${play}`)) {
      band.btn.down = false;
      band.sound?.kill(killFade);
      band.sound = null;
    }
  });

  // Scrolling.
  if (e.is("scroll")) {
    console.log("Scroll:", e);
    // TODO: Check to see if cursor is on top of one of the bands to activate
    //       scrolling.
  }
}

function sim() {
  bands.forEach((band) => {
    if (band.upping) {
      band.tone = min(band.tone + 1, toneHigh);

      band.sound?.update({
        tone: band.tone,
        duration: 0.05,
      });
    }

    if (band.downing) {
      band.tone = max(band.tone - 1, toneLow);
      band.sound?.update({
        tone: band.tone,
        duration: 0.05,
      });
    }
  });
}

// 📚 Library

// function beat() {
//   // Runs once per system metronome (BPM) tick.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
