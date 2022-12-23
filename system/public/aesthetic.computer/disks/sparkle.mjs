// Sparkle, 22.12.19.07.28
// Add sparkles to an image, with graphics by Molly Soda.

/* #region 🏁 todo
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

let tf;

const keys = ["a", "e", "i", "o", "p", "q", "r", "t", "u", "w", "y"];
const size = 13;
const scale = 2;

// 🎨 Paint (Executes every display frame)
export function paint({ pen, num: { randIntRange, randInt }, ink, net }) {
  if (!tf) tf = new Typeface(net.preload, sparkles, "sparkles");

  if (pen?.drawing) {
    ink(
      randIntRange(200, 250),
      randIntRange(200, 250),
      randIntRange(200, 250),
      150
    ).printLine(
      keys[randInt(keys.length - 1)],
      tf.glyphs,
      pen.x - (size * scale) / 2,
      pen.y - (size * scale) / 2,
      6,
      scale,
      0
    );
  }

}

export const system = "nopaint";