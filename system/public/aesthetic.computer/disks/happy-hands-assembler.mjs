// Happy Hands Assembler, 23.04.24.15.02
// Let's a some happy hands!

/* #region 🤝 Read Me 
#endregion */

const boxSize = 5;
const boxType = "fill*center";

function digit() {
  const p = () => ({ x: 0, y: 0, z: 0 });
  return [p(), p(), p(), p()];
}

//hand structure
const hand = {
  w: [{ x: 0, y: 0, z: 0 }],
  t: digit(),
  i: digit(),
  m: digit(),
  o: digit(),
  p: digit(),
};

const handPalette = {
  w: "#FFFFFFFF",
  t: [0, 170, 200],  //teal
  i: [75, 0, 130],    //indigo
  m: "magenta",   
  o: "orange",   
  p: "pink"  //pink
};

const nudge = 10;
const hnudge = nudge / 4;

//thumb
hand.t[0].x -= nudge;
hand.t[0].y -= nudge;
hand.t[1].x -= nudge * 2;
hand.t[1].y -= nudge * 2;
hand.t[2].x -= nudge * 3;
hand.t[2].y -= nudge * 3;
hand.t[3].x -= nudge * 4;
hand.t[3].y -= nudge * 4;
//index
hand.i[0].x -= hnudge;
const inudge = 14;
const basei = nudge * 4;
hand.i[0].y -= basei;
hand.i[1].x -= hnudge * 1.3;
hand.i[1].y -= basei + inudge;
hand.i[2].x -= hnudge * 1.7;
hand.i[2].y -= basei + inudge * 2;
hand.i[3].x -= hnudge * 2;
hand.i[3].y -= basei + inudge * 3;
//middle
hand.m[0].x -= hnudge - 14;
const mnudge = 15;
const basem = nudge * 4;
hand.m[0].y -= basem;
hand.m[1].x -= hnudge - 14;
hand.m[1].y -= basem + mnudge;
hand.m[2].x -= hnudge - 14;
hand.m[2].y -= basem + mnudge * 2;
hand.m[3].x -= hnudge - 14;
hand.m[3].y -= basem + mnudge * 3;
//ring
hand.o[0].x -= hnudge - 27;
const onudge = 14;
const baseo = nudge * 4;
hand.o[0].y -= baseo;
hand.o[1].x -= hnudge * -0.2 - 27;
hand.o[1].y -= baseo + onudge;
hand.o[2].x -= hnudge * -1.3 - 27;
hand.o[2].y -= baseo + onudge * 2;
hand.o[3].x -= hnudge * -1.7 - 27;
hand.o[3].y -= baseo + onudge * 3;
//pinky
hand.p[0].x -= hnudge -38;
const basep = nudge *3.5; 
const pnudge = nudge;
hand.p[0].y -= basep;
hand.p[1].x -= pnudge * -0.6 - 38;
hand.p[1].y -= basep + pnudge;
hand.p[2].x -= pnudge * -1.3 - 38;
hand.p[2].y -= basep + pnudge * 2;
hand.p[3].x -= pnudge * -1.7 - 38;
hand.p[3].y -= basep + pnudge * 3;

const hands = 1024; // How many happy hands exist in total?
const key = "happy-hand-assembler:hand"; // Keep track of current hand index.

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ wipe, params, screen, store }) {
  let h = parseInt(params[0]);

  if (isNaN(h)) {
    const stored = await store.retrieve(key, "local:db");
    h = stored;
  }

  if (h === null || isNaN(h) || h === undefined || h < 0 || h > hands - 1) {
    console.warn("👎 Hand Not Found:", h);
    wipe(100, 0, 0)
      .ink(120, 0, 0)
      .line(0, 0, screen.width, screen.height)
      .line(0, screen.height, screen.width, 0);
  } else {
    // 🤚 We have a hand!
    wipe(0, 64, 0)
      .ink(0, 255, 0, 128)
      .write(h, { x: 4, y: screen.height - 13 });
    store[key] = h;
    store.persist(key, "local:db");
  }
}
// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, box, line, pan, unpan, screen }) {
  wipe(0); // draw bg
  pan(screen.width / 2 - 20, screen.height / 2 + 40); // shift view

  // 🅱️ Hand Lines
  // ...


  // 🅰️ Hand Points
  ink(handPalette.w).box(hand.w[0].x, hand.w[0].y, boxSize, boxType); // wrist 
  ink(handPalette.t); //thumb
  for (let coord of hand.t) box(coord.x, coord.y, boxSize, boxType); 
  ink(handPalette.i); // index
  for (let coord of hand.i) box(coord.x, coord.y, boxSize, boxType); 
  ink(handPalette.m); // middle
  for (let coord of hand.m) box(coord.x, coord.y, boxSize, boxType); 
  ink(handPalette.o); // ring (o)
  for (let coord of hand.o) box(coord.x, coord.y, boxSize, boxType); 
  ink(handPalette.p); // pinky
  for (let coord of hand.p) box(coord.x, coord.y, boxSize, boxType); 
  unpan();
}

export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
// ...

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/
