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
        sig: "paint(api)",
        desc: "Runs once when a piece starts.",
      },
      act: {
        label: "🎪 Act",
        sig: "act(api)",
        desc: "Runs once when a piece starts.",
      },
      sim: {
        label: "🧮 Sim",
        sig: "sim(api)",
        desc: "Runs once when a piece starts.",
      },
      beat: {
        label: "🥁 Beat",
        sig: "beat(api)",
        desc: "Runs once when a piece starts.",
      },
      leave: {
        label: "👋 Leave",
        sig: "leave(api)",
        desc: "Runs once when a piece starts.",
      },
      meta: {
        label: " 📰 Meta",
        sig: "meta(api)",
        desc: "Runs once when a piece starts.",
      },
      preview: {
        label: "🖼️ Preview",
        sig: "preview(api)",
        desc: "Runs once when a piece starts.",
      },
      icon: {
        label: "🪷 Icon",
        sig: "icon(api)",
        desc: "Runs once when a piece starts.",
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
        desc: "",
      },
    },
    // Commands for entering into the prompt.
    prompt: {
      no: {
        sig: "no",
        desc: "",
      },
    },
  });
}
