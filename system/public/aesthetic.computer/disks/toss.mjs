// Toss, 2024.11.24.01.04.50.399
// Play two oscillators at once.

/* 📝 Notes
 */

let lbtn, rbtn;

function makeButtons({ screen, ui }) {
  const hw = screen.width / 2;
  lbtn = new ui.Button(0, 0, hw, screen.height);
  rbtn = new ui.Button(hw, 0, hw, screen.height);
}

function boot({ api }) {
  makeButtons(api);
}

function paint({ api, wipe, ink, line, screen, box, circle, pen, write }) {
  wipe("purple"); // Clear the background.

  [lbtn, rbtn].forEach((btn) => {
    const col = btn === lbtn ? "orange" : "green";
    btn.paint((b) => {
      ink(b.down ? col : "black").box(b.box.x, b.box.y, b.box.w, b.box.h);
    });
  });

  const gap = 32;

  ink("red");
  write(ltone, gap, gap);

  ink("red");
  write(rtone, screen.width / 2 + gap, gap);
}

const startTone = 1000;
let lsound, rsound;
let ltone = startTone,
  rtone = startTone;

function act({ event: e, sound }) {
  // Respond to user input here.
  [lbtn, rbtn].forEach((btn, index) => {
    const left = index === 0;

    btn.act(e, {
      down: () => {
        const osc = sound.synth({
          type: "sine",
          tone: left ? ltone : rtone, // TODO: ltone;
          duration: "🔁",
        });

        if (left) {
          lsound = osc;
        } else {
          rsound = osc;
        }
      },
      up: () => {
        if (left) {
          lsound.kill();
        } else {
          rsound.kill();
        }
      },
    });
  });

  if (e.is("draw")) {
    if (lbtn.down) {
      ltone += e.delta.y;
      lsound.update({
        tone: ltone,
        duration: 0.05,
      });
    } else if (rbtn.down) {
      rtone += e.delta.y;
      rsound.update({
        tone: rtone,
        duration: 0.05,
      });
    }
  }
}

// 📚 Library

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

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
