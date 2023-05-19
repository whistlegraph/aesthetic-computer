// Vary, 23.05.17.02.07
// Vary an existing image with AI.

/* #region ✅ TODO 
  - [] Upload and receive image from `api/vary`.
#endregion */

let picture;
let message = "PROCESSING...";

const failure = "NETWORK FAILURE";
const msgDelay = 250;

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ system, encode, net }) {
  // Encode a png from the current painting and upload it to the server.
  const png = await encode({ file: system.painting, type: "png" });
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
function paint({ ink, system }) {
  if (message) {
    ink(0xcccccc).write(message, { x: 7, y: 24 });
    system.nopaint.needsPresent = true; // This should be a more simple flag.
    //                                     And perhaps be automatic and
    //                                     tied to the return value of paint?
  }
}

function bake({ paste }) {
  if (picture) {
    paste(picture, 0, 0);
    picture = null; // Only run once.
  }
}

export const system = "nopaint";
export { boot, paint, bake };

// 📚 Library (Useful functions used throughout the piece)
