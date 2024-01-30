// Docs, 24.01.29.21.54
// Return up to date data for the aesthetic.computer pieces api.

/* #region 🏁 TODO 
#endregion */

import { respond } from "../../backend/http.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  return respond(200, {
    top: {
      // 🧩 Top Level Piece Functions
      boot: {
        label: "🥾 Boot",
        sig: "boot({ ... })",
        desc: "Runs once when a piece starts.",
      },
      paint: {
        label: "🎨 Paint",
        sig: "paint({ ... })",
        desc: "Repeatedly draw to the screen at the hardware refresh rate.",
      },
      act: {
        label: "🎪 Act",
        sig: "act({ ... })",
        desc: "Respond to user and system input.",
      },
      sim: {
        label: "🧮 Sim",
        sig: "sim({ ... })",
        desc: "For calculations occurring once per logic frame. (120fps)",
      },
      beat: {
        label: "🥁 Beat",
        sig: "beat({ ... })",
        desc: "Runs once per system metronome tick, for rhythmic coordination.",
      },
      leave: {
        label: "👋 Leave",
        sig: "leave({ ... })",
        desc: "Execute code right before the piece is unloaded.",
      },
      meta: {
        label: " 📰 Meta",
        sig: "meta({ ... })",
        desc: "Runs once when a piece starts.",
      },
      preview: {
        label: "🖼️ Preview",
        sig: "preview({ ... })",
        desc: "Paint a custom thumbnail image.",
      },
      icon: {
        label: "🪷 Icon",
        sig: "icon({ ... })",
        desc: "Paint a piece icon, AKA `favicon`",
      },
    },
    // Commands for programming inside of pieces.
    api: {
      // Graphics
      wipe: {
        sig: "wipe(color)",
        desc: "Paint all pixels the same `color`.",
      },
      ink: {
        sig: "ink(color)",
        desc: "Select a `color` for painting with.",
      },
      line: {
        sig: "line(x1, y1, x2, y2)",
        desc: "Paint straight a 1px line from two points.",
      },
    },
    // Pieces that can be entered into the prompt.
    pieces: {
      line: {
        sig: "line:thickness color",
        desc: "Paint freehand lines in any `thickness` or `color`",
      },
      rect: {
        sig: "rect color",
        desc: "Paint rectangles in any `color`",
      },
    },
    // Commands for entering into the prompt.
    prompt: {
      no: {
        sig: "no",
        desc: "Undo the last step of the system painting.",
      },
      yes: {
        sig: "yes",
        desc: "Redo the last step of the system painting.",
      },
    },
  });
}
