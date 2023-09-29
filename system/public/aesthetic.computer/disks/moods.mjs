// Moods, 2023.9.28.01.40.14.735
// A live list of all our moods.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Move extra long moods left and right horizontally.
  - [] Add time information.
  - [] Filter out new line characters.
  + Done
  - [x] Make moods scrollable by line.
  - [x] Render moods.
#endregion */

let retrieving = true;
let failed = false;
let moods;
const scrollDelay = 60;
let scroll = -scrollDelay;
const { floor, min, abs, ceil } = Math;

let moodRing = [];
const moodRingY = 18;
const moodRingRow = 12;
let ringSpots = 8;
let calcRingSpots;

// 🥾 Boot
function boot({ wipe, ink, line, screen }) {
  // Runs once at the start.
  wipe(0);

  fetch("/api/mood/all")
    .then((res) => res.json())
    .then((body) => {
      if (body.moods) {
        console.log("Moods:", body);
        moods = body.moods;

        calcRingSpots = (screen) => {
          ringSpots = ceil(
            min(moods.length, (screen.height - moodRingY) / moodRingRow),
          );
          console.log("new ring spots:", ringSpots);
          for (let i = moodRing.length - 1; i < ringSpots; i += 1) {
            moodRing.push(moods.shift());
          }
        };

        calcRingSpots(screen);
        retrieving = false;
      } else {
        throw new Error(body.message);
      }
    })
    .catch((err) => {
      failed = true;
      retrieving = false;
      console.warn("📶🙁 Mood error:", err);
    });
}

// 🎨 Paint
function paint({ wipe, ink, text, pan, unpan, screen }) {
  wipe(0);
  if (retrieving) ink(255).write("retrieving...", { center: "xy" });
  if (failed) ink("red").write("failed", { center: "xy" });
  if (moodRing.length > 0) {
    ink(255, 0, 0, 64).line(0, moodRingY, screen.width, moodRingY);
    if (scroll < 0)
      ink(255, 0, 0, 128).line(
        0,
        moodRingY,
        screen.width * (abs(scroll) / scrollDelay),
        moodRingY,
      );
    if (scroll > 0) pan(0, -floor(scroll));
    moodRing.forEach((m, i) => {
      const mood = m.mood.trim();
      const bounds = text.box(mood);
      // console.log(bounds);
      const y = moodRingY + i * moodRingRow;
      ink().write(mood, { x: 6, y });
      ink(64).write(m.handle, { x: 6 + bounds.box.width - 6, y });
    });
    unpan();

    scroll += 0.5;
    if (scroll > moodRingRow) {
      scroll = 0;
      moods.push(moodRing.shift());
      moodRing.push(moods.shift());
    }
    //scroll = (scroll - 0.5) % (moodRing.length * 12);
    ink(0, 127).box(0, 0, screen.width, moodRingY);
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("reframed")) calcRingSpots?.(screen);
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Moods",
    desc: "A live list of all our moods.",
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

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
