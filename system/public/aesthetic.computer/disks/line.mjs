// Line, 22.09.19.12.44
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region 🏁 todo
  + Future
  - [] Better freehand smoothing across the board.
  - [] Transform hold line before committing a gesture / making the next line?
  - [] More optimized rendering / shader options. 
  + Done
  - [x] Add line thickness.
    - [x] Add a thick preview.
    - [x] Or a colon parameter.
  - [x] Paste line automatically before stroke ends / optimize for longer
        strokes, but keep pixel perfect algorithm...
  - [x] Implement `pppline` over `line`.
    - [x] Add subtle race based line smoothing...
#endregion */

let points = []; // Gesture data.
let bake = false; // Flag for baking strokes to the painting.
let pparams; // Processed parameters.
let race;
let thickness;

const debug = false;

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
  thickness = parseInt($.colon[0]) || 1; // Set line thickness with a colon param.

  // Set up line smoothing system.
  let step, speed;
  if (thickness === 1) {
    step = 3;
    speed = 40;
  } else {
    step = thickness / 4;
    speed = 30;
  }
  // Ick: This should be refactored. 23.02.01.14.09
  race = new $.geo.Race({ step, speed, quantized: true });
}

// 🧮
function sim({ num, pen }) {
  if (pen?.drawing) {
    const to = race.to();
    if (to?.out) {
      // Ick: This is pretty ugly API retrofitting / un-ergonomic. 23.02.01.13.23
      const qp = to.out[0];
      if (qp) addPoint(num, ...qp);
    }
  }
}

// 🎨
function paint({ pen, ink, num, paste, page, system, screen }) {
  if (bake) {
    page(system.painting); // Paste last preview line to painting.
    paste(screen);
    page(screen);
    bake = false;
  }

  // Render an in-progress stroke.
  if (pen?.drawing) {
    paste(system.painting);

    const color = (pos, pix, col, vcol) => {
      // ✨ Randomly add some sparkles.
      if (false /*Math.random() > 0.90*/) {
        num.blend(pix, [...num.randIntArr(255, 3), col[3]]);
      } else if (vcol) {
        num.blend(pix, vcol); // Shade pixel based on vertex color.
      } else if (col) {
        num.blend(pix, col); // Shade pixel based on ink color.
      }
    };

    // Draw the current gesture up to the current pen point.
    if (thickness === 1) {
      ink(pparams).pppline([...points.slice(), pen], { color });
    } else {
      ink(pparams).pline([...points.slice(), pen], thickness, { color });
      // TODO: How could I derive the preview points from this so that I can
      //       cache the tail of a gesture periodically to a bitmap? Do I even
      //       want this or would GPU offloading be even better here? 23.02.01.17.22
    }

    if (debug && race.pos)
      ink(255, 0, 0).box(race.pos[0] - 2, race.pos[1] - 2, 5); // Graph race dot.
  }
}

// ✒ Act
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.
  const { event: e, pen, num } = $;

  if (e.is("touch:1") && pen) {
    points.length = 0;
    race.start([pen.x, pen.y]);
    addPoint(num, pen.x, pen.y);
  }

  if (e.is("draw:1") && pen) race.goal = [pen.x, pen.y];
  if (e.is("lift:1")) bake = true;
}

export { boot, paint, sim, act };

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)

function addPoint(num, x, y) {
  let color;
  if (pparams.length === 0) color = num.randIntArr(255, 3);
  points.push({ x, y, color }); // Push last point.
}
