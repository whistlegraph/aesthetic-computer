// Paint, 23.05.15.00.01
// Ask a LLM to produce code that automatically paints the request!

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
  - [X] Request cancellation handling on the server via `ask.js`.
  - [x] Use GPT-3.5.
  - [x] Always print the full source code.
  - [x] Segmented / incremental input.
  - [x] Confine to canvas. 
  - [x] Pipe in `/llm` post request and pass in information.
  - [x] Eval the request.
#endregion */

let brush,
  code = "",
  fullCode = "PROCESSING...",
  lines = [],
  controller;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ params, system: { painting }, needsPaint }) {
  const program = {
    before: `you must tell a virtual grid where to put colored rectangles to describe the following, you can do this by sending responses of individual lines consisting of: "ink(r, g, b, a).box(x, y, w, h)" where r, g, b, and a range from 50 to 255 and x, y is within the integer resolution of ${painting.width}, ${painting.height} and w, h ranges from 0 to 128. boxes draw from the top left. never use 255 or 0 for a! now try to plot an image of... in a way that a human would recognize as the subject visually`,
    after: `make your response no longer than 20 lines where each line ends in a semicolon. choose colors related to the subject and draw clearly. all boxes should fit completely within the frame - every line of your response must begin with "ink" and nothing else`,
  };

  ask(
    {
      prompt: params.join(" ") || "a red circle",
      program,
      hint: "code",
    },
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
        lines.push(line);
        code = code.slice(semicolon + 1); // Remove the line from `code`.
      }
    },
    function finished() {
      fullCode = ``;
      // code = ``;
    },
    function failed() {
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
  controller?.abort(); // Cancel any existing `ask` which halts the server.
}

export const system = "nopaint";
export { boot, paint, bake, leave };

// 📚 Library (Useful functions used throughout the piece)

// Query a LLM
// `options` can be a string prompt or an object { prompt, program }
// where `program` has a `before` and `after` string.
async function ask(options, and, finished, failed) {
  let prompt,
    program = { before: "", after: "" };
  if (typeof options === "string") {
    prompt = options;
  } else {
    ({ prompt, program } = options);
  }

  controller?.abort(); // Prevent multiple asks / cancel existing ones.
  controller = new AbortController();
  const signal = controller.signal;

  try {
    const host = DEBUG
      ? "http://localhost:3000"
      : "https://ai.aesthetic.computer";

    const responsePromise = fetch(`${host}/api/ask`, {
      method: "POST",
      signal,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ prompt, program }),
    });

    let timeout;

    const timeoutPromise = new Promise((resolve, reject) => {
      timeout = setTimeout(() => {
        controller.abort();
        reject(new Error(`Reply timed out after 10 seconds!`));
      }, 10000);
    });

    clearTimeout(timeout);
    const response = await Promise.race([responsePromise, timeoutPromise]);

    if (!response.ok) throw new Error(`Failed to reply: ${response.status}`);

    const readableStream = response.body;
    const decoder = new TextDecoder();

    const reader = readableStream.getReader();

    // Detect chunks of JSON as they stream in.
    while (true) {
      const { done, value } = await reader.read();

      if (done) {
        if (DEBUG) console.log("❗ Response complete.");
        controller = null;
        finished?.();
        break;
      }

      const got = decoder.decode(value, { stream: true }); // Chunk to text.
      and?.(got);
    }
  } catch (error) {
    console.error("Failed to ask:", error);
    failed?.();
  }
}
