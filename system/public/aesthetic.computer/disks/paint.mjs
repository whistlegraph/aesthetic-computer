// Paint, 23.05.15.00.01
// Ask a LLM to produce code that automatically paints the request!

/* #region 📓 TODO 
  + Now
  - [] Confine to canvas. 
  + Done
  - [x] Pipe in `/llm` post request and pass in information.
  - [x] Eval the request.
#endregion */

let code, brush;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ params, system: { painting } }) {
  const program = {
    before: `you must tell a virtual grid where to put colored rectangles to describe the following, you can do this by sending responses of individual lines consisting of: "ink(r, g, b, a).box(x, y, w, h)" where r, g, b, and a range from 0 to 255 and x, y is within the integer resolution of ${painting.width}, ${painting.height} and w, h ranges from 0 to 128. boxes draw from the top left. never use 255 or 0 for a! if no location is discussed, randomize the x and y a lot!`,
    after: `make your response no longer than 10 lines`,
  };

  code = (await ask(params.join(" ") || "a red circle", program)).trim();
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, system }) {
  if (code) {
    try {
      console.log("Running:", code);
      const fun = new Function("ink", code);
      brush = () => {
        fun(ink);
        brush = null;
      };
      system.nopaint.needsBake = true;
    } catch (error) {
      console.error("Generation error:", error);
    }
    code = null;
  }
}

// 🍪 Prints to the current painting.
function bake() {
  brush?.();
}

export const system = "nopaint";

export { boot, paint, bake };

// 📚 Library (Useful functions used throughout the piece)

async function ask(prompt, program = { before: "", after: "" }) {
  try {
    const response = await fetch("/llm", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ prompt, program }),
    });

    if (!response.ok) {
      throw new Error(`Failed to ask, HTTP Error: ${response.status}`);
    }

    const data = (await response.json()).reply;
    return data;
  } catch (error) {
    console.error("Failed to ask:", error);
  }
}