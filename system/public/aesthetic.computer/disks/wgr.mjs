// Whistlegraph Recorder, 22.12.27.19.30
// A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  - [] Lines break / crash at the top of the screen...
  - [-] Add line drawing with pan, zoom, and thickness support.
    - [] Fill in each quad once 4 points are added.
      - [] Use triangles?
    - [] Draw triangulated / filled end-caps.
    - [-] Support thickness.
    - [x] Add two parallel points for each coordinate at a time,
          starting from the second.
    - [x] Show a live preview.
    - [x] Add pixel-perfect single lines in a display list / abstracted graph.
  - [] Add microphone input. 
  - [] Add audio and video recording. 
    - [] Record video
    - [] with microphone
    - [] Add background beat (via parameter?)
    - [] Finish video saving UI.
  - [] Make the background cool and grainy / animated a bit.
#endregion */

class Gesture {
  $; // API
  points = []; // Each point contains `{x, y, pressure}` data.
  thickness = 24;

  constructor($, point) {
    this.$ = $;
    this.add(point);
  }

  add(next) {
    const last = this.points[this.points.length - 1] || next;
    if (
      this.points.length === 0 ||
      this.$.num.dist(next.x, next.y, last.x, last.y) > this.thickness
    ) {
      this.points.push(next);
    }
  }
}

class Whistlegraph {
  $; // API
  gestures = [];
  gestureIndex = -1;

  constructor($) {
    this.$ = $;
  }

  touch(x, y, pressure) {
    this.gestures.push(new Gesture(this.$, { x, y, pressure }));
    this.gestureIndex = this.gestures.length - 1;
  }

  draw(x, y, pressure) {
    // TODO: Get distance between last points...
    this.gestures[this.gestureIndex].add({ x, y, pressure });
  }

  preview($, next) {
    const g = this.gestures[this.gestureIndex];
    const cur = g.points[g.points.length - 1];
    const segment = [cur, next];
    $.ink([200, 0, 0]);
    if (g.thickness === 1) $.pppline(segment);
    else $.pline(segment, g.thickness);
  }

  paint($) {
    this.gestures.forEach((g, index) => {
      const color = index === this.gestureIndex ? [128, 0, 0] : [64, 64, 64];
      $.ink(color);
      if (g.thickness === 1) $.pppline(g.points);
      else $.pline(g.points, g.thickness);
    });
  }
}

let wg;

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  $.wipe(255, 0, 0);
  wg = new Whistlegraph($);
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe, pen } = $;
  wipe(127);
  if (pen?.drawing) {
    wg.preview($, { x: pen.x, y: pen.y, pressure: pen.pressure });
  }
  wg.paint($);
}

// ✒ Act (Runs once per user interaction)
function act({ event: e }) {
  // Primary pointer
  if (e.is("touch:1")) {
    wg.touch(e.x, e.y, e.pressure);
  }

  if (e.is("draw:1")) {
    wg.draw(e.x, e.y, e.pressure);
  }

  //if (e.is("lift:1")) {}

  // Respond to user input here.
}

/*
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

// 📚 Library (Useful functions used throughout the piece)
export { boot, paint, act };
