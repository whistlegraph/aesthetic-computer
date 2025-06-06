// Paint, 23.05.15.00.01
// Ask Claude 4 Opus to produce code that automatically paints the request!

/* #region 📓 TODO
  + Now
  - [] Clean up code.
  - [] Fail if prompt is empty / provide a sane default.
  + Later
  - [] Add sound based on color?
  - [] Add line support.
  - [] Condense the syntax and adjust the prompt.
  - [] Make another prompt based tool or a conversation tool.
       (So this can be abstracted.)
  - [] How can I somehow make a character editor?
  - [] And have two chacters talk to one another in turns, adding a human?
  - [] ASCII graphics.
  + Done
  - [x] Add Claude 4 Opus model option via 'claude' flag.
  - [X] Request cancellation handling on the server via `ask.js`.
  - [x] Use GPT-3.5.
  - [x] Always print the full source code.
  - [x] Segmented / incremental input.
  - [x] Confine to canvas.
  - [x] Pipe in `/llm` post request and pass in information.
  - [x] Eval the request.
#endregion */

import { Conversation } from "../lib/ask.mjs";

let conversation,
  brush,
  code = "",
  fullCode = "PROCESSING...",
  lines = [],
  abort;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ params, system: { painting }, needsPaint, store, slug }) {
  const program = {
    before: `You must tell a virtual grid where to paint boxes (rectangles), circles and 1px thick lines, you can do this by sending responses of individual lines consisting of: ink(r, g, b, a).box(x, y, w, h) or ink(r, g, b, a).line(x1, y1, x2, y2) or ink(r, g, b, a).circle(x, y, radius)  where r, g, b, and a range from 50 to 255 and x, y is within the integer resolution of ${painting.width}, ${painting.height} and w, h ranges inside that resolution. never use 255 or 0 for a! Now try to plot an image of... `,
    // user input
    after: `... and plot this in a way that a human would recognize as the subject visually - Usually place any object in the center. Make your response no longer than 50 lines where each line ends in a semicolon, but it can be shorter. choose colors related to the subject and draw clearly. All coordinates should fit completely within the frame resolution. Every line of your response must begin with "ink" and nothing else. Do not use markdown formatting.`,
  };

  conversation = new Conversation(store, slug);
  await conversation.retrieve();

  abort = conversation.ask(
    { prompt: params.join(" ") || "a red circle", program, hint: "code" },
    function and(msg) {
      if (fullCode === "PROCESSING...") {
        fullCode = ``; // Clear any waiting message.
      }
      // console.log("🗨️", msg); // Every new response shows up here.
      code += msg;
      fullCode += msg;
      // Remove anything in `code` up to and including the first semicolon.
      const semicolon = code.indexOf(";"); // Get index of the first semicolon.
      if (semicolon !== -1) {
        const line = code.slice(0, semicolon + 1); // A full statement.
        lines.push(line.trim() + "\n"); // Trim and add a line terminating character.
        code = code.slice(semicolon + 1); // Remove the line from `code`.
      }
    },
    function done() {
      fullCode = ``;
    },
    function fail() {
      fullCode = "NETWORK FAILURE";
    }
  );
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, system }) {
  if (fullCode) {
    fullCode
      .trim()
      .split("\n")
      .forEach((line, row) => {
        ink(0xcccccc).write(line, { x: 7, y: 24 + row * 12 });
      });
    system.nopaint.needsPresent = true; // This should be a more simple flag.
  }

  if (lines.length > 0) {
    let fun;
    try {
      fun = new Function("ink", lines.join("\n")); // Wrap 1+ statements.
      brush = () => {
        try {
          fun(ink);
        } catch (err) {
          console.log("Failed to execute:", fun);
        }
        brush = null;
      };
      system.nopaint.needsBake = true;
    } catch (err) {
      console.log("Failed to interpret:", fun);
    }
    lines.length = 0; // Clear line buffer.
  }
}

// 🍪 Prints to the current painting.
function bake() {
  brush?.();
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

export const system = "nopaint";
export { boot, paint, bake, leave };

// 📚 Library (Useful functions used throughout the piece)
