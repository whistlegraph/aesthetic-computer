// Moods, 2023.9.28.01.40.14.735
// A live list of all our moods.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Move extra long moods left and right horizontally.
  - [] Add time information.
  - [] Filter out new line characters.
  + Done
  - [x] Make moods scrollable by line.
  - [x] Render moods.
#endregion */

let moods;

let retrieving = true,
  failed = false;

const scrollDelay = 60;
let scroll = -scrollDelay;
let scale = 1;

let moodRing = [],
  moodRingY = 18,
  moodRingRow = 12;

const blockWidth = 6 * scale;
let bounceCount = 1;

let ringSpots = 8,
  calcRingSpots;

const { floor, min, abs, ceil, sin } = Math;

// 🥾 Boot
function boot({ wipe, screen, colon, params }) {
  scale = parseInt(colon[0]) || 1;
  if (scale === 2) moodRingY *= scale / 1.5;
  moodRingRow *= scale;
  wipe(0);
  let query = `/api/mood/all`;
  if (params[0]) query += `?for=${params[0]}`;
  fetch(query)
    .then((res) => res.json())
    .then((body) => {
      if (body.moods) {
        moods = body.moods;
        // console.log("😃 Moods:", moods);
        calcRingSpots = (screen) => {
          ringSpots = ceil(
            min(moods.length, (screen.height - moodRingY) / moodRingRow) + 3,
          );
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
function paint({ wipe, ink, text, pan, unpan, screen, num, help: { choose } }) {
  scale > 1 ? ink(0, 64).box(screen) : wipe(0);
  if (retrieving) ink(choose(64, 127)).write("Retrieving...", { center: "xy" });
  if (failed) ink("red").write("failed", { center: "xy" });
  if (moodRing.length > 0) {
    bounceCount += 1;
    ink(255, 200, 200, 64).line(0, 0, screen.width, 0);
    if (scroll < 0)
      ink().line(0, 0, screen.width * (abs(scroll) / scrollDelay), 0);
    if (scroll > 0) pan(0, -floor(scroll));
    moodRing.forEach((m, i) => {
      const mood = m.mood.trim();
      const mb = text.box(mood, undefined, undefined, scale).box.width;
      const hb = text.box(m.handle, undefined, undefined, scale).box.width;
      const y = moodRingY + i * moodRingRow;
      let x = 6;
      const totalWidth = mb + hb - blockWidth;
      if (totalWidth > screen.width) {
        const gap = totalWidth - screen.width;
        const osc = (sin(num.radians(bounceCount % 360)) + 1) / 2;
        x = x - gap + osc * gap;
      }
      ink(scroll > 0 ? undefined : "brown").write(mood, { x, y, size: scale });
      ink(64).write(m.handle, { x: x + mb - blockWidth, y, size: scale });
    });
    unpan();
    scroll += 0.5;
    if (scroll > moodRingRow * 2) {
      scroll = moodRingRow;
      moods.push(moodRing.shift());
      moodRing.push(moods.shift());
    }
    if (scroll && scale === 1) ink(0, 127).box(0, 0, screen.width, moodRingY);
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("reframed")) calcRingSpots?.(screen);
}

// 🧮 Sim
function sim() {
  if (scroll < 0) bounceCount += 0.25;
}

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
function preview({ wipe, slug }) {
  wipe("purple").ink("red").write(slug, { center: "xy", size: 1 });
}

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
