// No Paint, 2023.7.15.23.42.19
// Press Paint if you like what you see or No if you don't.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let brush;
const apis = [];
const cycle = 800;
let cycleFrame = 0;

// 🥾 Boot
async function boot({ api, wipe, ink, line, net }) {
  const name = "rect"; // Load the brush.
  brush = await import(`${net.pieces}/${name}.mjs`);

  // Set the params for the brush.
  const bootApi = { ...api };
  bootApi.params = [];
  brush?.boot?.(bootApi); // Initialize the brush.
}

// 🧮 Sim
function sim({ num: { randIntRange: rr }, screen: { width, height }, system }) {
  if (cycleFrame < cycle) {
    if (cycleFrame === 0) {
      const w = rr(8, 64),
        h = rr(8, 64),
        x = rr(-w / 2, width - w / 2),
        y = rr(-h / 2, height - h / 2),
        dragBox = { x, y, w, h };
      const api = { pen: { x, y, dragBox } };
      apis.push(api);
    }
    cycleFrame += 1;
  }
}

// 🎨 Paint
function paint($) {
  apis.forEach((api) => {
    // Translate the pen to the current painting position.
    api.system = {
      nopaint: {
        is: (state) => state === "painting",
        translation: { ...$.system.nopaint.translation },
      },
    };
    // TODO: Add the `mode` in here
    //       for the gesture.
    $.system.nopaint.updateBrush(api); // Set the current transform of brush.

    const passApi = { ...$ };
    passApi.system = api.system; // Replace with our generated system.
    passApi.pen = api.pen;
    brush?.paint?.(passApi);
  });
  // apis.length = 0; // Consume any simulated apis.
}

// 🎪 Act
function act({ event: e }) {
  // Respond to user input here.
  // if (e.is("keyboard:down:p")) {
  // brush?.bake?.();
  // }
}

// 🍪 Prints to the current painting.
function bake() {
  brush?.bake?.();
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
    title: "No Paint",
    desc: "Press Paint if you like what you see or No if you don't.",
  };
}

export const system = "nopaint:bake-on-leave";

export { boot, sim, paint, act, bake, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
