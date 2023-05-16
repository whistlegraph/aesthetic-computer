// Paint, 23.05.15.00.01
// Ask a LLM to produce code that automatically paints the request!

/* #region 📓 TODO 
  + Now
  - [-] Always print the full source code.
  - [] Add GPT-3.5 / chatting flag. (Model picker.)
  + Later
  - [] Request cancellation handling on the server via `ask.js`.
  + Done
  - [x] Segmented / incremental input.
  - [x] Confine to canvas. 
  - [x] Pipe in `/llm` post request and pass in information.
  - [x] Eval the request.
#endregion */

let brush,
  code = "PROCESSING...",
  lines = [],
  controller;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ params, system: { painting }, needsPaint }) {
  const program = {
    before: `you must tell a virtual grid where to put colored rectangles to describe the following, you can do this by sending responses of individual lines consisting of: "ink(r, g, b, a).box(x, y, w, h)" where r, g, b, and a range from 50 to 255 and x, y is within the integer resolution of ${painting.width}, ${painting.height} and w, h ranges from 0 to 128. boxes draw from the top left. never use 255 or 0 for a! now try to plot an image of... in a way that a human would recognize as the subject visually`,
    after: `make your response no longer than 20 lines where each line ends in a semicolon. choose colors related to the subject and draw clearly. all boxes should fit completely within the frame - every line of your response must begin with "ink" and nothing else`,
  };

  ask(
    { prompt: params.join(" ") || "a red circle", program },
    function and(msg) {
      if (code === "PROCESSING...") {
        code = ``; // Clear any waiting message.
      }
      // console.log("🗨️", msg); // Every new response shows up here.
      code += msg;
      // Remove anything in `code` up to and including the first semicolon.
      const semicolon = code.indexOf(";"); // Get index of the first semicolon.
      if (semicolon !== -1) {
        const line = code.slice(0, semicolon + 1); // A full statement.
        lines.push(line);
        code = code.slice(semicolon + 1); // Remove the line from `code`.
      }
    },
    function finished() {
      code = ``;
    },
    function failed() {
      code = "NETWORK FAILURE";
    }
  );
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, system }) {
  if (code) {
    code
      .trim()
      .split("\n")
      .forEach((line, row) => {
        ink(0xcccccc).write(line, { x: 7, y: 24 + row * 12 });
      });
    system.nopaint.needsPresent = true; // This should be a more simple flag.
  }

  if (lines.length > 0) {
    const fun = new Function("ink", lines.join("\n")); // Wrap 1+ statements.
    brush = () => {
      try {
        fun(ink);
      } catch (err) {
        console.log("Failed to interpret:", fun);
      }
      brush = null;
    };
    system.nopaint.needsBake = true;
    lines.length = 0; // Clear line buffer.
  }
}

// 🍪 Prints to the current painting.
function bake() {
  brush?.();
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  console.log("Aborting...");
  controller?.abort(); // Cancel any existing `ask.
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

    const responsePromise = fetch(
      `${host}/api/ask`,
      {
        method: "POST",
        signal,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ prompt, program }),
      },
    );

    const timeoutPromise = new Promise((resolve, reject) => {
      setTimeout(() => reject(new Error(`Reply timed out!`)), 10000);
    });

    const response = await Promise.race([responsePromise, timeoutPromise]);

    if (!response.ok) throw new Error(`Failed to reply: ${response.status}`);

    const readableStream = response.body;
    const decoder = new TextDecoder();

    const reader = readableStream.getReader();

    // Read the data incrementally as it arrives
    while (true) {
      const { done, value } = await reader.read();

      if (done) {
        if (DEBUG) console.log("❗ Response complete.");
        finished?.();
        break;
      }

      // 1 or more json chunks.
      const got = decoder.decode(value, { stream: true }); // Chunk to text.
      const chunks = got.match(/{[^{}]*}/g);
      const out = [];

      // Loop through each JSON chunk and parse it.
      for (const chunk of chunks) {
        try {
          const msg = JSON.parse(chunk);
          and?.(msg.text); // Run the call back for every message.
        } catch (err) {
          console.error("Failed to parse chunk as JSON:", err);
        }
      }
    }
  } catch (error) {
    if (DEBUG) console.error("Failed to ask:", error);
    failed?.();
  }
}
