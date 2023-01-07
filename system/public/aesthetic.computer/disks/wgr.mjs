// Whistlegraph Recorder, 22.12.27.19.30
// A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [] Make each line a different color.
  - [] Make each line a different stroke option.
  - [] Zoom (shift or two finger pinch changes relative thickness)
    - [🟠] Implement zoomFromPoint(n)` with positive or negative n.
      - [] Changes relative thickness.
    // TODO: How to improve zooming and also add rotating to the keyboard?
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
    - [] Rename `wgr` -> `whistlegraph` and remap `wg` to "learn whistlegraph",
         or something like that.
  - 🛑 Launch 1
  + Done
    - [x] Pan (space bar or two finger drag)
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

let wg, bg;
let ALT = false;
let SHIFT = false;
const pan = { x: 0, y: 0 };

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const { num, help, wipe } = $;
  // $.glaze({on: true});
  bg = num.randIntArr(128, 3);
  wipe(bg);
  wg = new Whistlegraph($, help.choose(1, 2));
}

// 🎨 Paint (Executes every display frame)
function paint($) {
  if (ALT || SHIFT) $.wipe(bg);
  wg.paint($);
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, pen, num: { p2 } }) {
  // ⌨️ Keyboard
  if (e.is("keyboard:down:alt")) ALT = true; // Panning ➡️
  if (e.is("keyboard:up:alt")) ALT = false;
  if (ALT && e.delta) p2.inc(wg.pan, e.delta);

  if (e.is("keyboard:down:shift")) SHIFT = true; // Zooming  🔭️
  if (e.is("keyboard:up:shift")) SHIFT = false;

  // TODO: How to improve zooming and also add rotating to the keyboard?
  if (SHIFT && e.delta) p2.inc(wg.zoom, p2.mul(e.delta, {x: 0.001, y: 0.001}));

  // 🖐️ Touch + Mouse 🖱️
  if (e.is("touch:1")) wg.touch(e); // Primary 🤙
  if (e.is("draw:1")) wg.draw(e);
  if (e.is("lift:1")) wg.lift();
}

/*
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

  pan = { x: 0, y: 0 };
  zoom = { x: 1, y: 1 };

  constructor($, thickness = 2) {
    this.$ = $;
    this.baseColor = $.num.randIntArr(255, 3);
    this.thickness = thickness;
  }

  project(p) {
    p = this.$.num.p2.mul(p, this.zoom); // Zoom
    return this.$.num.p2.add(p, this.pan); // Pan
  }

  unproject(p) {
    p = this.$.num.p2.sub(p, this.pan); // Unpan
    return this.$.num.p2.div(p, this.zoom); // Unzoom
  }

  touch({ x, y, pressure }) {
    this.baseColor = this.$.num.randIntArr(255, 3);
    this.genColor();

    this.gestures.push(
      new Gesture(
        this.$,
        { ...this.unproject({ x, y }), pressure, color: this.currentColor },
        this.thickness
      )
    );
    this.gestureIndex = this.gestures.length - 1;
  }

  draw({ x, y, pressure }) {
    this.genColor();
    // this.baseColor = this.$.num.randIntArr(255, 3);
    this.gestures[this.gestureIndex].add({
      ...this.unproject({ x, y }),
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
          ...this.unproject($.pen),
          pressure: $.pen.pressure,
          color: wg.currentColor,
        }
      : undefined;

    for (let i = this.gestures.length - 1; i >= 0; i -= 1) {
      const g = this.gestures[i];

      // const color = i === this.gestureIndex ? [128, 0, 0] : [64, 64, 64];
      $.ink(this.baseColor);

      // Collect and view-transform all geometry for a given gesture.
      const points = (
        next && this.gestureIndex === i ? [...g.points, next] : g.points
      ).map((p) => this.project(p)); // Transform every point.

      // Render each gesture.
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
