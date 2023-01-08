// Whistlegraph Recorder, 22.12.27.19.30
// A simple, 2D tool for recording whistlegraphs.

/* #region 🏁 todo
  + ⏰ Now
  - [] Zoom (shift or two finger pinch changes relative thickness)
    - [🟠] Implement zoomFromPoint(n)` with positive or negative n.
      - [] Changes relative thickness.
    // TODO: How to improve zooming and also add rotating to the keyboard?
    - [] Add input events that trigger it.
      - [] What about two keyboard shortcuts also? 
      - [] Will trackpad zoom work? 
  - [] Make each line a different color.
  - [] Make each line a different stroke option.
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
    - [x] Add pixel-perfect single lin- [] RGB Black line tool (with thickness).
- [] RGBA Fill bucket.
- [] Frame is transparent.
- [] Safe zones.es in a display list / abstracted graph.
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

// 🎨 Paint
function paint($) {
  if (ALT || SHIFT) $.wipe(bg);
  wg.paint($);
  // $.ink(255, 255, 0, 128).box(wg.anchor.x, wg.anchor.y, 8, "out*center");
}

// 🧮 Sim(ulate)
function sim({ num: { mat3, p2 }, pen, screen }) {
  if (pen) {
    if (ALT) wg.anchor({x: pen.x, y: pen.y});
    if (SHIFT) wg.zoom({ x: 1.001, y: 1.001 });
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, pen, num: { p2 } }) {
  if (e.is("keyboard:down:alt")) ALT = true; // Panning ➡️
  if (e.is("keyboard:up:alt")) ALT = false;
  if (ALT && e.delta) wg.move(e.delta);
  // if (SHIFT && e.device === "mouse") wg.anchor = { x: e.x, y: e.y };

  if (e.is("keyboard:down:shift")) SHIFT = true; // Zooming  🔭️
  if (e.is("keyboard:up:shift")) SHIFT = false;

  if (e.is("touch:1")) wg.touch(e); // Drawing 🤙
  if (e.is("draw:1")) wg.draw(e);
  if (e.is("lift:1")) wg.lift();
}

export { boot, paint, act, sim };

// 📚 Library (Useful functions used throughout the piece)

// A composition of gestures.
class Whistlegraph {
  $; // API
  gestures = [];
  gestureIndex = -1;
  baseColor;
  currentColor;
  thickness;
  matrix;

  pan = { x: 0, y: 0 };
  scale = { x: 1, y: 1 };

  anchorPoint = { x: 0, y: 0 };
  oldAnchor = { x: 0, y: 0 };
  lastScale = { x: 1, y: 1 };

  constructor($, thickness = 2) {
    this.$ = $;
    this.baseColor = $.num.randIntArr(255, 3);
    this.thickness = thickness;
    this.matrix = $.num.mat3.create();
  }

  // Pans around the view by a screen-coordinate amount.
  move(p) {
    const pp = this.$.num.p2.mul(p, { x: 1 / this.scale.x, y: 1 / this.scale.y });
    this.$.num.p2.inc(this.pan, pp);
  }

  anchor(p) {
    const {
      num: { vec2, mat3 },
    } = this.$;
    // const newScreenAnchor = {x: p.x, y: p.y};
    // const newWorldAnchor = this.unproject(newScreenAnchor);

    const newScreenAnchor = { x: p.x, y: p.y };
    const newWorldAnchor = this.unproject(newScreenAnchor);

    const anchorDiff = {
      x: this.anchorPoint.x - newWorldAnchor.x,
      y: this.anchorPoint.y - newWorldAnchor.y,
    };

    // const sd = this.$.num.p2.mul(anchorDiff, { x: this.scale.x, y: this.scale.y });

    // this.pan = {
    //     x: this.pan.x - sd.x,
    //     y: this.pan.y - sd.y,
    //  };

    this.anchorPoint = newWorldAnchor;
  }

  zoom(amt, p) {
    this.lastScale = { x: this.scale.x, y: this.scale.y };
    const newScale = this.$.num.p2.mul(this.scale, {
      x: amt.x || amt,
      y: amt.y || amt,
    });

    this.scale = newScale;
  }

  // -> Projects a whistlegraph point onto the screen. (world to screen)
  project(p) {
    const {
      num: { vec2, mat3 },
    } = this.$;
    const m = mat3.create(); // Identity
    mat3.translate(m, m, [this.anchorPoint.x, this.anchorPoint.y]); // Unanchor
    mat3.scale(m, m, [this.scale.x, this.scale.y]); // Scale
    // TODO: Rotate
    mat3.translate(m, m, [-this.anchorPoint.x, -this.anchorPoint.y]); // Anchor
    mat3.translate(m, m, [this.pan.x, this.pan.y]); // Pan
    const tp = vec2.transformMat3(vec2.create(), [p.x, p.y], m);
    return { x: tp[0], y: tp[1] };
  }

  // <- Unprojects a screen location into whistlegraph view. (screen to world)
  unproject(p) {
    const {
      num: { vec2, mat3 },
    } = this.$;
    const m = mat3.create(); // Identity
    mat3.translate(m, m, [-this.pan.x, -this.pan.y]); // Pan
    mat3.translate(m, m, [this.anchorPoint.x, this.anchorPoint.y]); // Unanchor
    mat3.scale(m, m, [1 / this.scale.x, 1 / this.scale.y]); // Scale
    // TODO: Rotate
    mat3.translate(m, m, [-this.anchorPoint.x, -this.anchorPoint.y]); // Anchor
    const tp = vec2.transformMat3(vec2.create(), [p.x, p.y], m);
    return { x: tp[0], y: tp[1] };
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
