// Word, 23.02.11.13.38
// Write a word (or words) on the screen that can be dragged around.

/* #region 🏁 todo
  + Second Version
    + System
    - [] Support question mark params on both colon and on color.
    - [] (Implement here then generalize for other brushes that parse
          color.) See `line` for an existing implementation.
  + Done
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

let text, size, color, x, y;

const words = ["sad", "OK", "i'm so creative", "alright", "NEXT LEVEL"];

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  $.system.nopaint.boot($); // Inherit boot functionality.
  const { params, colon, help, screen } = $;

  // Parse all parameters.
  const sep = '"'; // The character to use as a separator.
  if (typeof params[0] === "string") {
    const singleWord = () => {
      if (params[0].endsWith(sep)) params[0] = params[0].slice(0, -1);
      text = params[0];
      color = params.slice(1).map((str) => parseInt(str));
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
        color = params.slice(end + 1).map((str) => parseInt(str));
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

  size = parseInt(colon) || 1; // No 0, undefined or NaN.

  x = screen.width / 2; // Default position is the center of the display.
  y = screen.height / 2;
}

// 🎨 Paint (Executes every display frame)
function paint({ params, pen, system, paste, ink, screen }) {
  const shadow = 1;
  paste(system.painting);
  ink(0, 50).write(text, { x: x + shadow, y: y + shadow, center: "xy", size });
  ink(color).write(text, { x, y, center: "xy", size });
}

// ✒ Act (Runs once per user interaction)
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.
  if ($.event.is("draw")) {
    x += $.pen.delta.x;
    y += $.pen.delta.y;
  }
}

export const system = "nopaint";

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint, act };
