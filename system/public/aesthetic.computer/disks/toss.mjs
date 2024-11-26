// Toss, 2024.11.24.01.04.50.399
// Play two oscillators at once.

/* 📝 Notes

  - [] Add keyboard shortcuts for the strips.
  - [] Add auto-resizing to buttons.


  - [] Do full volume at center of strip and less on the edges.

  - [x] Add attack and decay similar to `notepat`.
  - [x] Make it so there can be N strips and they can be layed out either
       "along" or "across" like `toss across` and have a number like `toss across 8` for 8 strips where dragging
       left and right changes the freq.
  - [x] Add vertical separator for the two strips.
 */

let bands = [];
let type;
const startTone = 440;
const attack = 0.005;
const killFade = 0.3;

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

const { min } = Math;

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
    bands[i].btn = new ui.Button(i * segWidth, 0, segWidth, screen.height);
  }
}

let bandCount;

function boot({ api, params }) {
  makeBands(api, min(shortcuts.length, parseInt(params[0]) || 2));
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write }) {
  wipe("purple"); // Clear the background.
  bands.forEach((band, index) => {
    const btn = band.btn;
    const col = index === 0 ? "orange" : "green";
    btn.paint((b) => {
      ink(b.down ? col : "black").box(b.box.x, b.box.y, b.box.w, b.box.h);
      if (b.box.x !== 0) {
        ink("white", 128).line(b.box.x, b.box.y, b.box.x, b.box.y + b.box.h);
      }
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
        scrub: (g) => {
          band.tone -= e.delta.y;
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
      band.tone += 1;
      band.sound?.update({
        tone: band.tone,
        duration: 0.05,
      });
    }

    if (band.downing) {
      band.tone -= 1;
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
