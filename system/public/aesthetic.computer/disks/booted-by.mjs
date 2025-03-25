// booted-by, 2023.9.13.19.43.52.028
// Aesthetic Computer was booted by...

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add a bar for Eric and a bar for Matt. 
  + Done
  - [x] Comment out the modal. 
  - [x] Remove prior empty claim slots. 
  - [x] Add a new message honoroing patrons.
  - [x] Send to Sean.
  - [x] add screen flash
  - [x] names and "unclaimed" or "empty" or "open" for open slots
  - [x] tapping bar brings back overlay
  - [x] 10 tappable bars with decoration.
  - [x] text jeffrey at xxx-xxx-xxxx to claim spot. urgent, legitimate, exciting 
  - [x] overlay with text and buttons
  - [x] ok button and demo video link within the text that takes you out of app to demo video 
#endregion */

import * as starfield from "./starfield.mjs";
import * as sfx from "./common/sfx.mjs";

const blockWidth = 6;
const blockHeight = 11;
const barColors = [
  "red",
  "orange",
  "green",
  "blue",
  "indigo",
  "grey",
  "violet",
  "white",
  "brown",
  "purple",
];
let ok,
  demo,
  bars = [];
let overlay = false;
let startupSfx;
let claim = true,
  claimBlink;
let flashing = false;

const copy = `We're at the beginning of a computer age where the advent of AI and text-to-media interfaces means that every user will become a programmer.\n\nAesthetic Computer aims to lead this paradigm shift as an accessible and evolving social platform for art and media creation.`;

const { min } = Math;

// 🥾 Boot
function boot({ ui, net, help }) {
  ok = new ui.TextButton("Fund");
  demo = new ui.TextButton("Demo");

  net.preload("startup").then((sfx) => (startupSfx = sfx)); //  Load startup

  barColors.forEach((color) => {
    bars.push({ tb: new ui.TextButton("CLAIM"), color });
  });

  starfield.boot();
}

// 🎨 Paint
function paint({
  api,
  ink,
  help,
  text,
  screen,
  help: { choose },
  num: { randIntArr },
}) {
  starfield.paint(api); // Starfield

  // Bars
  const gap = 4;
  const rowH = bars[0].tb.height + gap;

  const flashColor = overlay ? [0, 0] : [...randIntArr(255, 3), 255];

  function paintBars(bars, x) {
    const totalHeight = rowH * bars.length;
    bars.forEach((bar, index) => {
      let color = [bar.color, flashColor, flashColor, bar.color];
      if (!claim && !overlay) color = ["green", "white", "white", "green"];
      bar.tb.paint(api, color);

      let text;
      if (bar.color === "red") {
        text = "Mitchell F. Chan";
      } else if (bar.color === "blue") {
        text = "Sean Moss-Pultz";
      } else if (bar.color === "indigo") {
        text = "Charles Huang";
      } else if (bar.color === "violet") {
        text = "Julia Yerger";
      } else if (bar.color === "white") {
        text = "Anthony Zollo";
      } else if (bar.color === "green") {
        text = "@wiltchamberlain";
      } else if (bar.color === "orange") {
        text = "caesuras";
      } else if (bar.color === "grey") {
        text = "Artur Matveichenkov";
      } else if (bar.color === "brown") {
        text = "Eric Doyle";
      } else if (bar.color === "purple") {
        text = "Matt Doyle & Yuehao Jiang";
      } else {
        text = bar.tb.btn.down ? undefined : claim ? "CLAIM" : "$10k+";
      }

      bar.tb.reposition(
        {
          x: x - bar.tb.width / 2,
          y: index * rowH + (screen.height / 2 - totalHeight / 2),
          screen,
        },
        text,
      );

      if (text === "@wiltchamberlain") {
        ink()
          .pan(choose(0, 1, -1), choose(0, 1, -1))
          .box(bar.tb.btn.box, "out")
          .unpan()
          .ink()
          .pan(choose(0, 1, -1), choose(0, 1, -1))
          .box(bar.tb.btn.box, "out")
          .unpan();
      }

      if (text === "Sean Moss-Pultz") {
        ink()
          .pan(choose(0, 1, -1), choose(0, 1, -1))
          .box(bar.tb.btn.box, "out")
          .unpan();
      }

    });
  }

  if (rowH * bars.length > screen.height) {
    const rowDist = 60; // Two vertical rows.
    paintBars(bars.slice(0, 5), screen.width / 2 - rowDist);
    paintBars(bars.slice(-5), screen.width / 2 + rowDist);
  } else {
    paintBars(bars, screen.width / 2); // Single vertical row.
  }

  if (!overlay) {
    ink(help.choose([200], [160])).write(
      "These original patrons helped boot Aesthetic Computer.",
      { center: "x", y: screen.height - 54 },
      "black",
      screen.width - 32,
    );
  }

  if (overlay) {
    const marg = screen.width < 250 ? 8 : 12;
    const bound = min(320, screen.width - blockWidth * 4);
    const x = screen.width / 2 - bound / 2,
      y = blockHeight * 2.5;

    const pos = { x, y, screen }; // Copy
    const tb = text.box(copy, pos, bound, 1);
    tb.box.height += 11;
    pos.y = screen.height / 2 - tb.box.height / 2;
    tb.box.y = pos.y;

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
    const buttonMarg = 9;
    ok?.reposition({ bottom: -28, right: buttonMarg, screen: tb.box });
    ok?.paint(api, [[0, 128, 0], "lime", "lime", [0, 128, 0]]);

    demo?.reposition({ bottom: -28, left: buttonMarg, screen: tb.box });
    demo?.paint(api, [[0, 0, 128], 200, 200, [0, 0, 128]]);
  }

  if (flashing) ink().box(0, 0, screen.width, screen.height);
}

// 🎪 Act
function act({ event: e, jump, sound, gizmo, seconds, delay }) {
  if (overlay) {
    ok?.btn.act(e, {
      down: () => sfx.down(sound),
      push: () => {
        sfx.push(sound);
        sound.play(startupSfx); // Play startup sound...
        overlay = false;
        flashing = true;
        delay(() => {
          starfield.wipe(overlay);
          flashing = false;
          claimBlink = new gizmo.Hourglass(seconds(1.25), {
            flipped: () => {
              claim = !claim;
            },
            autoFlip: true,
          });
        }, 8);
        claim = true;
      },
    });
    demo?.btn.act(e, {
      push: () => {
        sfx.push(sound);
        jump(
          "https://calendly.com/aesthetic-computer/demo",
          // "https://www.dropbox.com/scl/fi/3cmnkp3oqoth9by99fieh/aesthetic-computer-demo.mov?rlkey=amzo78pi2qrrctiy434tle3nq&dl=0",
          // "out:https://www.tiktok.com/@whistlegraph/video/7281664540314438955",
        );
      },
    });
  } else {
    bars.forEach((bar) => {
      bar.tb.btn.act(e, {
        down: () => sfx.down(sound),
        push: () => {
          sfx.push(sound);
          // overlay = true;
          starfield.wipe(overlay);
        },
      });
    });
  }
}

// 🧮 Sim
function sim($) {
  starfield.sim($);
  claimBlink?.step();
}

// 📰 Meta
function meta() {
  return {
    title: "booted by",
    desc: "Aesthetic Computer was booted by...",
  };
}

// 🖼️ Preview
function preview({ wipe, slug }) {
  wipe(0, 125, 25).ink(0, 15, 25).write(slug, { center: "xy", size: 2 });
}

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, sim, meta, preview };

// 📚 Library
