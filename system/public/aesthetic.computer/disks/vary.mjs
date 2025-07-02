// Vary, 23.05.17.02.07
// Vary an existing image with AI.

/* #region ✅ TODO 
  + Now
  - [-] Get it working for all resolution ranges.
  - [*] Why isn't `ask` working locally? Check on mac...
    - [-] Maybe vercel is not a great choice and I have other
          options...
  - [-] Abstract "system message" logic.
  + Done
  - [x] Abstract the ellipsisTicker in `video` and add it here
       and to `paint`.
  - [x] Upload and receive image from `api/vary`.
#endregion */

let picture;
let message = "PROCESSING";

const failure = "NETWORK FAILURE";
const msgDelay = 250;
let ellipsisTicker;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ system, encode, gizmo }) {
  ellipsisTicker = new gizmo.EllipsisTicker();

  // Encode a png from the current painting and upload it to the server.
  const png = await encode({
    file: system.painting,
    type: "png",
    modifiers: { crop: "square" },
  });

  const formData = new FormData();
  formData.append("image", new File([png], "painting.png"));

  // Make a POST request to the API
  fetch(`/api/vary`, { method: "POST", body: formData })
    .then(async (response) => {
      if (!response.ok) {
        setTimeout(() => (message = failure), msgDelay);
        throw new Error(`Network response was not ok: ${response.json()}`);
      }
      return response.blob();
    })
    .then(async (data) => {
      // Turn the response into a `paste`able picture for `bake`.
      const bitmap = await createImageBitmap(data);
      const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
      const context = canvas.getContext("2d");
      context.drawImage(bitmap, 0, 0);
      const id = context.getImageData(0, 0, bitmap.width, bitmap.height);
      picture = { pixels: id.data, width: bitmap.width, height: bitmap.height };
      system.nopaint.needsBake = true;
      message = null; // Clear processing message.
    })
    .catch((error) => {
      setTimeout(() => (message = failure), msgDelay);
      console.error("There was a problem with the fetch operation:", error);
    });
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, system, help }) {
  if (message) {
    let m = message;
    if (message === "PROCESSING") m += ellipsisTicker.text(help.repeat);
    ink(0xcccccc).write(m, { x: 7, y: 24 });
    system.nopaint.needsPresent = true; // This should be a more simple flag.
    //                                     And perhaps be automatic and
    //                                     tied to the return value of paint?
  }
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ clock }) {
  ellipsisTicker.update(clock.time());
}

function bake({ paste }) {
  if (picture) {
    paste(picture, 0, 0);
    picture = null; // Only run once.
  }
}

export const system = "nopaint";
export { boot, paint, sim, bake };

// 📚 Library (Useful functions used throughout the piece)
// ...
