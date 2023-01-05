// Icon, 22.12.19.07.28
// Add icons to an image, with graphics by Molly Soda.

/* #region 🏁 todo
- [] Rename sparkles directories to icon.

- [] How can Amalia login...
  - [] Make plots and associate them with letters or dump them.
  - [] Browse, delete, and tag or rename them.
- [-] Load in glyphs.
- [-] Add drawing support. 
- [] Add keyboard key support.
- [] Add Amalia's actual test files and hook up to keys.
- [] Randomize color?
- [] Set up plots bucket on digital ocean?
#endregion */

import { Typeface } from "../lib/type.mjs";
import { sparkles } from "../disks/common/fonts.mjs";
const { sin, abs } = Math;

let tf;

const keys = ["a", "e", "i", "o", "p", "q", "r", "t", "u", "w", "y"];
const size = 13;
let scale = 2;


const key = 10;

let oscTimer = 0;

// Scale shift.

// 🎨 Paint (Executes every display frame)
export function paint({ system, paste, pen, num: { randIntRange, randInt }, ink, net, paintCount }) {
  if (!tf) tf = new Typeface(net.preload, sparkles, "sparkles");

  
  if (pen?.drawing) {
    paste(system.painting);
    scale = 1 + (abs(sin(0.05 * oscTimer)) * 4);
    oscTimer += 1;

    ink(
      randIntRange(200, 250),
      randIntRange(200, 250),
      randIntRange(200, 250),
      150
    ).printLine(
      keys[3],
      tf.glyphs,
      pen.x - (size * scale) / 2,
      pen.y - (size * scale) / 2,
      6,
      scale,
      0
    );
  } else {
    oscTimer = 0; // Move this to act.
  }

}

export const system = "nopaint";