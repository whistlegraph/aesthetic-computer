// Line, 22.09.19.12.44
// Inherits from the "nopaint" system, which predefines boot, act, and leave.

/* #region 🏁 todo
  + Future
  - [] Better freehand smoothing across the board.
  - [] Transform hold line before committing a gesture / making the next line?
  - [] More optimized rendering / shader options.
  + Done
  - [x] Full color support
  - [x] Adapt to new brush API.
  - [x] Add line thickness.
    - [x] Add a thick preview.
    - [x] Or a colon parameter.
  - [x] Paste line automatically before stroke ends / optimize for longer
        strokes, but keep pixel perfect algorithm...
  - [x] Implement `pppline` over `line`.
    - [x] Add subtle race based line smoothing...
#endregion */

let lines, // The baking function.
  colorParams, // Processed parameters.
  race,
  thickness;

const points = []; // Gesture data.
const debug = false;

// Export to the `about` piece / show documentation.
function about({ colon, params, num }) {
  const color = num.parseColor(params);
  let name = num.findColor(color);
  let alpha = 1;
  if (color.length === 2 || color.length === 4)
    alpha = (color[color.length - 1] / 255).toFixed(1);
  if (color.length === 2) {
    if (color[0] === 0) name = "black";
    else if (color[0] === 255) name = "white";
    else name = "gray";
  }

  // TODO: This should output a text-only version that can generically be
  //       *replaced* with paint code in a piece like `about` or on the prompt
  //       but pure text is always gonna be useful! 23.05.14.18.58
  // TODO: Return some lexical information like index of color start and end?
  if (!name) name = "*COLOR*";

  let text = `paint ${colon[0] || 1}px ${name} lines`;
  if (alpha < 1) text += ` with ${alpha} alpha`;
  return `${text}.`;
}

// 🥾
// If `params` is empty then ink's RGBA will be randomized every segment.
// If entering "?" then each indivudal param will be randomized once.
function boot({ params, geo, num, colon }) {
  colorParams = num.parseColor(params);

  thickness = parseInt(colon[0]) || 1; // Set line thickness with a colon param.

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
  race = new geo.Race({ step, speed, quantized: true });
}

// 🧮
function sim({ num, pen, system: { nopaint } }) {
  if (nopaint.is("painting")) {
    const to = race.to();
    if (to?.out) {
      // Ick: This is pretty ugly API retrofitting / un-ergonomic. 23.02.01.13.23
      const qp = to.out[0];
      if (qp) addPoint(num, ...qp);
    }
  }
}

// 🎨
function paint({ pen, ink, num, system: { nopaint } }) {
  // Render an in-progress stroke.
  if (nopaint.is("painting")) {
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
      // console.log(pen, points);
      ink(colorParams).pppline([...points.slice(), pen], { color });
    } else {
      ink(colorParams).pline([...points.slice(), pen], thickness, { color });
      // TODO: How could I derive the preview points from this so that I can
      //       cache the tail of a gesture periodically to a bitmap? Do I even
      //       want this or would GPU offloading be even better here? 23.02.01.17.22
    } // UI

    const brush = nopaint.brush;

    lines = () => {
      // Transform existing points to the canvas.
      const tpoints = points.slice().map((p) => {
        const { x, y } = nopaint.transform(p);
        return { ...p, x, y };
      });

      if (thickness === 1) {
        ink(colorParams).pppline([...tpoints, brush], { color });
      } else {
        ink(colorParams).pline([...tpoints, brush], thickness, { color });
      }
    }; // Painting

    if (debug && race.pos)
      ink(255, 0, 0).box(race.pos[0] - 2, race.pos[1] - 2, 5); // Plot race dot.
  }
}

function bake({ paste, screen }) {
  lines?.();
  // paste(screen); // 📓 The old method was pasting a screen buffer.
  //                      This use case could return for some brushes...
}

// ✒ Act
function act({ event: e, pen, num }) {
  // Start .
  if (e.is("touch:1")) {
    // console.log("empty points");
    points.length = 0;
    race.start([e.x, e.y]);
    addPoint(num, e.x, e.y);
  }

  if (e.is("draw:1")) race.goal = [e.x, e.y];
}

function preview({ ink, wipe }) {
  wipe("red").ink("blue").write("line", { center: "xy" });
}

const system = "nopaint";

export { about, boot, paint, sim, act, bake, system, preview };

// 📚 Library (Useful functions used throughout the piece)

function addPoint(num, x, y) {
  let color;
  if (colorParams.length === 0) color = num.randIntArr(255, 3);
  if (colorParams[0] === "rainbow") color = "rainbow";
  points.push({ x, y, color }); // Push last point.
}
