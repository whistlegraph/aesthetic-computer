// Icon, 22.12.19.07.28
// Add vector glyph icons to an image, with graphics by Molly Soda.

/* #region 🏁 todo
- [-] Two finger placement for rotation and scale.
  - [] Re-loop / restart to stay in the flow?
- [] Make a little freehand drawing editor called `freeplot` or `graph`.
     (Plot is not good enough for these doodles.)
- [] Add command line parameters to specify glyphs...
  - [] `icon @mollysoda glyph-name`
+ Later
  - [] How can Amalia login...
    - [] Make plots and associate them with letters or dump them.
      - [] Set up plots bucket on digital ocean?
    - [] Browse, delete, and tag or rename them.
+ Done
  - [x] Randomize color.
  - [x] Add Amalia's actual test files.
  - [x] Load in glyphs.
  - [x] Add drawing support. 
  - [x] First tap is zooming in and out with an icon.
  - [x] Second is an adjustment loop.
  - [x] Rename sparkles directories to icon.
#endregion */

import { Typeface } from "../lib/type.mjs";
import { icons } from "../disks/common/fonts.mjs";
const { sin, abs } = Math;

let typeface;

const glyphs = ["a", "e", "i", "o", "p", "q", "r", "t", "u", "w", "y"];
let glyphIndex = 0;

const glyphSize = 13;
let glyphScale = 2;
let scaleTimer = 0;
let glyphPos;

let mode = 0; // 0 for instantiating, 1 for placing.

// 🥾
function boot($) {
  $.system.nopaint.boot($); // Inherit boot functionality.
  typeface = new Typeface($.net.preload, icons, "icons");
}

// 🎨 Paint (Executes every display frame)
function paint({
  system,
  paste,
  pen,
  num: { randIntRange, randInt },
  ink,
}) {
  if (pen?.drawing) {
    paste(system.painting); // 👮 Why is this not saving on live reload!

    if (mode === 0) {
      glyphScale = 1 + abs(sin(0.05 * scaleTimer)) * 4;
      scaleTimer += 1;
      // glyphPos = { x: randInt(screen.width), y: randInt(screen.height) };
      glyphPos = {x: pen.x, y: pen.y};
    }

    ink(
      randIntRange(200, 250),
      randIntRange(200, 250),
      randIntRange(200, 250),
      150
    ).printLine(
      glyphs[glyphIndex],
      typeface.glyphs,
      // pen.x - (glyphSize * glyphScale) / 2,
      // pen.y - (glyphSize * glyphScale) / 2,
      glyphPos.x - (glyphSize * glyphScale) / 2,
      glyphPos.y - (glyphSize * glyphScale) / 2,
      6,
      glyphScale,
      0
    );
  }
}

function act($) {
  $.system.nopaint.act($); // Inherit boot functionality.
  const {
    event: e,
    num: { randInt },
    screen,
  } = $;

  if (e.is("touch:1")) {
    if (mode === 0) {
      glyphIndex = randInt(glyphs.length - 1);
    }
  }

  if (e.is("draw:1")) {
    if (mode === 1) {
      glyphPos.x += e.delta.x;
      glyphPos.y += e.delta.y;
    }
  }

  if (e.is("lift:1")) {
    if (mode === 0) {
      scaleTimer = 0;
      mode = 1;
    } 
  }
}

export { boot, paint, act };
export const system = "nopaint";