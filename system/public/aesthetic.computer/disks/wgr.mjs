// Whistlegraph Recorder, 22.12.27.19.30
// A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  - [🟡] Add "stroke" smoothing / tail for fast drawing curves.
    - [] Make a `geo.race` and visualize it.
    - [] This should also apply to the pppline, and pppline
         should get shader support as well!
  - [-] Add line drawing with pan, zoom, and thickness support.
    - [] Pan (space bar or two finger drag)
    - [] Zoom (shift or two finger pinch changes relative thickness)
      - [] Implement 1zoomFromPoint(n)` with positive or negative n.
        - [] Changes relative thickness.
      - [] Add input events that trigger it.
        - [] What about two keyboard shortcuts also? 
        - [] Will trackpad zoom work? 
  - [] Add microphone input. 
  - [] Make the background cool and grainy / animated a bit.
  - [] Add audio and video recording. 
    - [] Record video
      - [] This will be done with `wgr seconds` in addition to a countdown timer.
        - [] If no seconds are entered, then no video recording occurs and
             a practice state is assumed!
    - [] With microphone.
    - [] Add background beat / music?
      - [] (via parameter #2)
    - [] Finish video saving UI.
      - [] Store to temporary online bucket and allow
           user to download / show code. 
  + Done
  - [x] Fill in each quad once 4 points are added.
    - [x] Use triangles?
  - [x] Support thickness.
  - [x] Add two parallel points for each coordinate at a time,
        starting from the second.
  - [x] Show a live preview.
  - [x] Add pixel-perfect single lines in a display list / abstracted graph.
- [x] Lines break / crash at the top of the screen...
  + Later
  - [] Store data in user account if someone is logged in.
  - [] Draw triangulated / filled end-caps / work more on geometry?
#endregion */

class Gesture {
  $; // API
  points = []; // Each point contains `{x, y, pressure}` data.
  thickness = 2;

  constructor($, point) {
    this.$ = $;
    this.add(point);
  }

  add(next) {
    const last = this.points[this.points.length - 1] || next;
    if (
      this.points.length === 0 ||
      this.$.num.dist(next.x, next.y, last.x, last.y) > this.thickness / 2
    ) {
      this.points.push(next);
    }
  }
}

class Whistlegraph {
  $; // API
  gestures = [];
  gestureIndex = -1;
  baseColor;
  currentColor;

  constructor($) {
    this.$ = $;
    this.baseColor = $.num.randIntArr(255, 3);
  }

  touch(x, y, pressure) {
    this.baseColor = this.$.num.randIntArr(255, 3);
    this.genColor();
    this.gestures.push(
      new Gesture(this.$, {
        x,
        y,
        pressure,
        color: this.currentColor,
      })
    );
    this.gestureIndex = this.gestures.length - 1;
  }

  draw(x, y, pressure) {
    this.genColor();
    // this.baseColor = this.$.num.randIntArr(255, 3);
    this.gestures[this.gestureIndex].add({
      x,
      y,
      pressure,
      color: this.currentColor,
    });
  }

  lift() {
    // Remove gesture on lift if there is only one point.
    const g = this.gestures[this.gestureIndex];
    if (g.points.length === 1) this.gestures.splice(this.gestureIndex);
  }

  paint($, next) {
    for (let i = this.gestures.length - 1; i >= 0; i -= 1) {
      const g = this.gestures[i];
      const color = i === this.gestureIndex ? [128, 0, 0] : [64, 64, 64];
      $.ink(color);
      const points =
        next && this.gestureIndex === i ? [...g.points, next] : g.points;
      if (g.thickness === 1) $.pppline(points);
      else {
        const {
          help: { choose },
          num: { clamp, lerp, randInt, rand },
          screen: { width, height },
        } = $;

        // TODO: - [] Perhaps this process could be slowed down?
        $.pline(points, g.thickness, function shade(p, c, b) {
          // 🪄 Position & Color Software Shader

          // Position (spread)
          const r1 = randInt(4);
          const r2 = randInt(4);
          p.x = clamp(p.x + choose(-r1, r1) * rand(), 0, width);
          p.y = clamp(p.y + choose(-r2, r2) * rand(), 0, height);

          // Color
          c[0] = clamp( lerp(b[0], p.y, 0.5) + choose(-1, 1), 0, 255 / (rand() * 2.2));
          c[1] = clamp( lerp(b[1], p.y, 0.5) + choose(-1, 1), 0, 255 / (rand() * 2.2));
          c[2] = clamp( lerp(b[2], p.x, 0.5) + choose(-1, 1), 0, 255 / (rand() * 2.2));

          // Sparkles
          if (Math.random() > 0.96) {
            c[0] = lerp(b[0], randInt(255), rand() / 2);
            c[1] = lerp(b[1], randInt(255), rand() / 2);
            c[2] = lerp(b[2], randInt(255), rand() / 2);
          }

        });
      }
    }
  }

  genColor() {
    this.currentColor = this.baseColor.map((c) => this.$.wiggle(c, 0.2, 3));
  }
}

let wg;

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  // $.glaze({on: true});
  $.wipe($.num.randIntArr(128, 3));
  wg = new Whistlegraph($);
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  const { wipe, pen } = $;
  // wipe(64);
  //$.noise16DIGITPAIN();
  let next = pen?.drawing
    ? {
        x: pen.x,
        y: pen.y,
        pressure: pen.pressure,
        color: wg.currentColor,
      }
    : undefined;
  wg.paint($, next);
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

  if (e.is("lift:1")) {
    wg.lift();
  }

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
