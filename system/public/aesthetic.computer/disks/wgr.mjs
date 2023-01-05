// Whistlegraph Recorder, 22.12.27.19.30
// A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [📗] Pan (space bar or two finger drag)
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
  + Later
    - [] Parameterize some ink / thickness options in the CLI.
    - [] Does there need to be a secondary buffer for the current stroke...
      - Not if the strokes stay fuzzy!
      - But this would make sense to add in `line`.
    - [] Add "stroke" curve / spline to smooth the input data.
    - [] Store data in user account if someone is logged in.
  - 🛑 Launch 1
  + Done
    - [x] Abstract pixel shaders into 'position' and 'color' shaders. 
    - [x] pppline should get shader support as well!
    - [x] pppline needs to be able to draw back to front.
    - [x] Fill in each quad once 4 points are added.
      - [x] Use triangles?
    - [x] Support thickness.
    - [x] Add two parallel points for each coordinate at a time,
          starting from the second.
    - [x] Show a live preview.
    - [x] Add pixel-perfect single lines in a display list / abstracted graph.
    - [x] Lines break / crash at the top of the screen...
  + Post-launch
    - [] Add button to switch inks.
    - [] Improve stroke rendering features. 
      - [] Rounded end-caps in pline? // Merge pline and pppline?
#endregion */

const { floor } = Math;

let wg;

// TODO: Create secondary stroke buffer which clears on resize?

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const { num, help, wipe } = $;
  // $.glaze({on: true});
  wipe(num.randIntArr(128, 3));
  wg = new Whistlegraph($, help.choose(1, 2));
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  wg.paint($);
}

// ✒ Act (Runs once per user interaction)
function act({ event: e }) {
  if (e.is("touch:1")) wg.touch(e.x, e.y, e.pressure); // Primary 🤙
  if (e.is("draw:1")) wg.draw(e.x, e.y, e.pressure);
  if (e.is("lift:1")) wg.lift();
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

export { boot, paint, act };

// 📚 Library (Useful functions used throughout the piece)

// Keeps track of gesture geometry which every `Whistlegraph` has a list of.
class Gesture {
  $; // API
  points = []; // Each point contains `{x, y, pressure}` data.
  thickness;

  constructor($, point, thickness = 2) {
    this.$ = $;
    this.add(point);
    this.thickness = thickness;
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

// A composition of gestures.
class Whistlegraph {
  $; // API
  gestures = [];
  gestureIndex = -1;
  baseColor;
  currentColor;
  thickness;

  constructor($, thickness = 2) {
    this.$ = $;
    this.baseColor = $.num.randIntArr(255, 3);
    this.thickness = thickness;
  }

  touch(x, y, pressure) {
    this.baseColor = this.$.num.randIntArr(255, 3);
    this.genColor();
    this.gestures.push(
      new Gesture(
        this.$,
        { x, y, pressure, color: this.currentColor },
        this.thickness
      )
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

  paint($) {
    const {
      help: { choose },
      num: { clamp, lerp, randInt, rand },
    } = $;

    const next = $.pen?.drawing
      ? {
          x: $.pen.x,
          y: $.pen.y,
          pressure: $.pen.pressure,
          color: wg.currentColor,
        }
      : undefined;

    for (let i = this.gestures.length - 1; i >= 0; i -= 1) {
      const g = this.gestures[i];
      const color = i === this.gestureIndex ? [128, 0, 0] : [64, 64, 64];
      $.ink(color);
      const points =
        next && this.gestureIndex === i ? [...g.points, next] : g.points;
      if (g.thickness === 1)
        $.pppline(points, {
          position: (pos) => {
            // 🩰 Spread
            const r1 = randInt(1);
            const r2 = randInt(1);
            pos.x = pos.x + choose(-r1, r1) * rand();
            pos.y = pos.y + choose(-r2, r2) * rand();
          },
          color: (pos, pix, col) => {
            // ✨ Sparkles
            if (Math.random() > 0.96) {
              pix[0] = randInt(255);
              pix[1] = randInt(255);
              pix[2] = randInt(255);
            } else {
              pix[0] = col[0];
              pix[1] = col[1];
              pix[2] = col[2];
            }
          },
        });
      else {
        $.pline(points, g.thickness, {
          position: (pos) => {
            // 🩰 Spread
            const r1 = randInt(4);
            const r2 = randInt(4);
            pos.x = pos.x + choose(-r1, r1) * rand();
            pos.y = pos.y + choose(-r2, r2) * rand();
          },
          color: (pos, pix, col) => {
            // 📊 Axis Gradient
            const axes = ["y", "y", "x"];
            for (let i = 0; i < 3; i += 1) {
              pix[i] = clamp(
                lerp(col[i], pos[axes[i]], 0.5) + choose(-1, 1),
                0,
                255 / (rand() * 2.2)
              );
            }
            // ✨ Sparkles
            if (Math.random() > 0.96) {
              pix[0] = lerp(col[0], randInt(255), rand() / 2);
              pix[1] = lerp(col[1], randInt(255), rand() / 2);
              pix[2] = lerp(col[2], randInt(255), rand() / 2);
            }
          },
        });
      }
    }
  }

  genColor() {
    this.currentColor = this.baseColor.map((c) => this.$.wiggle(c, 0.2, 3));
  }
}
