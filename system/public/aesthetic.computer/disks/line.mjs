// Line, 22.09.19.12.44
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region 🏁 todo
  - [❤️‍🔥] Paste line automatically before stroke ends / optimize for longer
       strokes, but keep pixel perfect algorithm...
  - [] Add line thickness...
  + Done
  - [x] Implement `pppline` over `line`.
    - [x] Add subtle race based line smoothing...
#endregion */

let points = []; // Gesture data.
let bake = false; // Flag for baking strokes to the painting.
let pparams; // Processed parameters.
let race;

// 🥾
// If `params` is empty then ink's RGBA will be randomized every segment.
// If entering "?" then each indivudal param will be randomized once.
function boot($) {
  $.system.nopaint.boot($); // Inherit boot functionality.
  // If any params are "?" then set them to random values & make everything int.
  pparams = $.params.map((str) => {
    if (str === "?") return $.num.randInt(255);
    else return parseInt(str);
  });

  // Set up line smoothing system.

  const step = 3;
  const speed = 40;
  race = new $.geo.Race({ step, speed, quantized: true });
}

// 🧮
function sim({ num }) {
  const to = race.to();
  if (to?.out) {
    const qp = to.out[1];
    let color;
    if (pparams.length === 0) color = num.randIntArr(255, 3);
    points.push({ x: qp[0], y: qp[1], color }); // Push last point.
  }
}

// 🎨
function paint({ pen, ink, num, paste, page, system, screen }) {
  if (bake) {
    page(system.painting).paste(screen).page(screen); // Paste line to painting.
    bake = false;
  }

  // Render an in-progress stroke.
  if (pen?.drawing) {
    paste(system.painting);
    // Draw the current gesture up to the current pen point.
    ink(pparams).pppline([...points.slice(), pen], {
      color: (pos, pix, col, vcol) => {
        // ✨ Randomly add some sparkles.
        if (false /*Math.random() > 0.90*/) {
          num.blend(pix, [...num.randIntArr(255, 3), col[3]]);
        } else if (vcol) {
          num.blend(pix, vcol); // Shade pixel based on vertex color.
        } else if (col) {
          num.blend(pix, col); // Shade pixel based on ink color.
        }
      },
    });

    //if (points.length > 3) {
    //  bake = true;
    //  points = points.slice(-1);
    //}

    // Visualize race dot.
    // if (race.pos) ink(255, 0, 0).box(race.pos[0] - 2, race.pos[1] - 2, 5);
  }
}

// ✒ Act
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.
  const { event: e, pen, num } = $;

  if (e.is("touch:1")) {
    // Trigger last stroke (if any) for baking.
    if (points.length > 0) {
      points.length = 0;
      bake = true;
    }

    race.start([pen.x, pen.y]);
  }

  if (e.is("draw:1")) {
    race.goal = [pen.x, pen.y];
  }

  // if (e.is("lift:1")) {}
}

export { boot, paint, sim, act };

export const system = "nopaint";
