// Booted-by, 2023.9.13.19.43.52.028
// Aesthetic was booted by...

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
    - [] text jeffrey at xxx-xxx-xxxx to claim spot. urgent, legitimate, exciting 
    - [] 10 tappable bars with decoration.
  - [] names and "unclaimed" or "empty" or "open" for open slots
  - [] tapping bar brings back overlay
  + Done
  - [x] overlay with text and buttons
  - [x] ok button and demo video link within the text that takes you out of app to demo video 
  + Later?
  - [] Make this a nice payment page
  - [] date of boot after boot completed
  - [] names link to people's profiles, invitation to make profile somewhere
#endregion */

const copy = `We're at the beginning of a computer age where the advent of AI and text-to-media interfaces means that everyone can be a programmer.\n\nAesthetic computer aims to lead this paradigm shift as an accessible and evolving social platform for art and media creation. We need funding to usher in the new age.`;

const blockWidth = 6;
const blockHeight = 11;
const barColors = [
  "red",
  "orange",
  "yellow",
  "green",
  "blue",
  "indigo",
  "violet",
  "white",
  "grey",
  "brown",
];
let ok,
  demo,
  bars = [];
let overlay = true;
let startupSfx;
const { min } = Math;

import * as starfield from "./starfield.mjs";

// 🥾 Boot
function boot({ ui, net }) {
  ok = new ui.TextButton("Fund");
  demo = new ui.TextButton("Demo");

  net.preload("startup").then((sfx) => (startupSfx = sfx)); //  Load startup

  barColors.forEach((color) => {
    bars.push({ tb: new ui.TextButton("CLAIM"), color });
  });

  starfield.boot();
}

// 🎨 Paint
function paint({ api, ink, wipe, text, screen, num: { randInt, randIntArr } }) {
  starfield.paint(api); // Starfield

  // Bars
  const gap = 4;
  const rowH = bars[0].tb.height + gap;

  const flashColor = overlay ? [0, 0] : [...randIntArr(255, 3), 255];

  function paintBars(bars, x) {
    const totalHeight = rowH * bars.length;
    bars.forEach((bar, index) => {
      bar.tb.paint(api, [bar.color, flashColor, flashColor, bar.color]);
      bar.tb.reposition({
        x: x - bar.tb.width / 2,
        y: index * rowH + (screen.height / 2 - totalHeight / 2),
        screen,
      });
    });
  }

  if (rowH * bars.length > screen.height) {
    // Two vertical rows.
    const rowDist = 40;
    paintBars(bars.slice(0, 5), screen.width / 2 - rowDist);
    paintBars(bars.slice(-5), screen.width / 2 + rowDist);
  } else {
    // Single vertical row.
    paintBars(bars, screen.width / 2);
  }

  if (overlay) {
    const marg = 4;
    const bound = min(300, screen.width - blockWidth * 4);

    const x = screen.width / 2 - bound / 2,
      y = blockHeight * 2.5;

    // Copy
    const pos = { x, y, screen };
    const tb = text.box(copy, pos, bound, 1);
    tb.box.height += 38;
    pos.y = screen.height / 2 - tb.box.height / 2;
    tb.box.y = pos.y;

    // pos.center = "x";
    tb.box.x -= marg;
    tb.box.width += marg * 2;
    tb.box.y -= marg;
    tb.box.height += marg;

    ink(0, 32, 32, 230).box(screen); // Backdrop
    ink(0, 100, 0, 64).box(tb.box); // Text Box
    ink(40, 170, 40, 180).write(copy, pos, undefined, bound); // Text
    ink(255, 255, 0, 64).box(
      tb.box.x,
      tb.box.y,
      tb.box.width,
      tb.box.height,
      "inline",
    ); // Outline

    // Buttons
    ok?.reposition({ bottom: 6, right: 6, screen: tb.box });
    ok?.paint(api, [[0, 128, 0], "lime", "lime", [0, 128, 0]]);

    demo?.reposition({ bottom: 6, left: 6, screen: tb.box });
    demo?.paint(api, [[0, 0, 128], 200, 200, [0, 0, 128]]);
  }
}

// 🎪 Act
function act({ event: e, jump, sound }) {
  if (overlay) {
    ok?.btn.act(e, {
      down: () => downSound(sound),
      push: () => {
        pushSound(sound);
        sound.play(startupSfx); // Play startup sound...
        overlay = false;
        setTimeout(() => {
          starfield.wipe(overlay);
        }, 50);
      },
    });
    demo?.btn.act(e, {
      push: () => {
        pushSound(sound);
        jump(
          "out:https://www.tiktok.com/@whistlegraph/video/7281664540314438955",
        );
      },
    });
  } else {
    bars.forEach((bar) => {
      bar.tb.btn.act(e, {
        down: () => downSound(sound),
        push: () => {
          pushSound(sound);
          overlay = true;
          starfield.wipe(overlay);
        },
      });
    });

    // if (e.is("lift")) overlay = true;
  }
}

// 🧮 Sim
function sim($) {
  starfield.sim($);
}

// 📰 Meta
function meta() {
  return {
    title: "Booted by",
    desc: "Aesthetic was booted by...",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

function pushSound(sound) {
  sound.synth({
    type: "sine",
    tone: 800,
    attack: 0.1,
    decay: 0.99,
    volume: 0.75,
    duration: 0.005,
  });
}

function downSound(sound) {
  sound.synth({
    type: "sine",
    tone: 600,
    attack: 0.1,
    decay: 0.99,
    volume: 0.75,
    duration: 0.001,
  });
}
