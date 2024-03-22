// Word, 23.02.11.13.38
// Write a word (or words) on the screen that can be dragged around.

/* #region 🏁 todo
  + System
    - [] Improve line thickness code / consider growing from
         a structure / outlining the skeleton as a post process
         rather than using pline?
    - [] Support question mark params on both colon and on color.
    - [] (Implement here then generalize for other brushes that parse
          color.) See `line` for an existing implementation.
  + Done
    - [x] Fix multi-line offset bug.
    - [x] Upgrade to new `paint` api.
    - [x] Add new color parameters.
    - [x] Add thickness as a secondary colon parameter / colon param 2.
    - [x] 🐛 Dragging is inconsistent. 
    - [x] Word should start in a random place within width and height.
    - [x] Colored text with custom quoted messaged and specific scale. 
#endregion */

/* region docs 📚

  Usage:
  `word "hello guys" color`
  `word hello color`

/* #endregion

/* #region ✏️ todo 
 - [] Print text from parameter under the mouse.
#endregion */

let word, text, size, color, x, y;
let thickness = 1;

const words = ["sad", "OK", "i'm so creative", "alright", "NEXT LEVEL"];

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const { params, colon, help, screen, num } = $;

  // Parse all parameters.
  const sep = '"'; // The character to use as a separator.
  if (typeof params[0] === "string") {
    const singleWord = () => {
      if (params[0].endsWith(sep)) params[0] = params[0].slice(0, -1);
      text = params[0];
      color = num.parseColor(params.slice(1)); //.map((str) => num.parseColor(str));
    };

    const secondParamNaN = isNaN(parseInt(params[1]));
    const li = params.length - 1;

    const quoted = params[0].startsWith(sep);

    // Check for quoted (sep) parameters.
    if (quoted || secondParamNaN) {
      if (!quoted && secondParamNaN && params.length > 1) {
        // Assume the entire list of params is text, with no specified color.
        params[0] = '"' + params[0]; // Quote for processing.
        params[li] += '"';
      }

      // Remove opening separator if needed.
      if (params[0].startsWith(sep)) params[0] = params[0].substring(1);

      let end = -1;
      for (let i = 1; i < params.length; i += 1) {
        if (params[i].endsWith(sep)) {
          params[i] = params[i].slice(0, -1); // Remove separator.
          end = i;
          break;
        }
      }
      if (end === -1) {
        singleWord(); // Ending quote not found, assume a single word.
      } else {
        text = params.slice(0, end + 1).join(" ");
        color = num.parseColor(params.slice(end + 1)); //.map((str) => num.parseColor(str));
      }
    } else {
      // TODO: Look ahead to see if the next parameter can be parsed as an
      //       integer, and if it can't then treat every parameter
      //       as part of "text".

      singleWord(); // No quoted parameter.
    }
  } else {
    text = help.choose(...words);
  }

  size = parseInt(colon[0]) || 1; // No 0, undefined or NaN.
  thickness = parseInt(colon[1]) || thickness;

  x = screen.width / 2; // Default position is the center of the display.
  // y = screen.height / 2;
  // x = num.randInt(screen.width);
  // y = num.randInt(screen.height);
  y = 0;
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, system: { nopaint, painting } }) {
  // Rendering commands to write to screen and painting.
  function print({ x, y }) {
    // Shadow
    const shadow = 1;
    ink(0, 50).write(
      text,
      {
        x: x + shadow,
        y: y + shadow,
        center: "xy",
        size,
        thickness,
        rotation: 0,
      },
      undefined,
      painting.width * 0.85,
    );

    // Text
    ink(color).write(
      text,
      {
        x,
        y,
        center: "xy",
        size,
        thickness,
        rotation: 0,
      },
      undefined,
      painting.width * 0.85,
    );
  }

  print({ x, y }); // Draw everything to the screen.

  word = () => {
    print(nopaint.transform({ x, y }));
    word = null;
  }; // Painting: Write to the canvas permanently.
}

// 🍪 Prints to the current painting.
function bake() {
  word?.();
}

// ✒ Act (Runs once per user interaction)
function act($) {
  const { event: e } = $;
  if (e.is("draw")) {
    x += e.delta.x;
    y += e.delta.y;
  }
}

export const system = "nopaint:bake-on-leave";

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act, bake };
