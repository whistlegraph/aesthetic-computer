// Vary, 23.05.17.02.07
// Vary an existing image with AI.

/* #region ✅ TODO 
  - [] Upload and receive image from `api/vary`.
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ system }) {
  // TODO: Retrieve the painting as a PNG...


  // Convert the pixels data to a Blob object
  const blob = new Blob([system.painting.pixels], {
    type: "application/octet-stream",
  });

  const file = new File([blob], "painting.png"); // Make a `File` out of the image, to be consumed by the server.

  // Create a FormData object to hold the image blob and other data
  const formData = new FormData();
  formData.append("image", file);
  formData.append("width", system.painting.width);
  formData.append("height", system.painting.height);

  // Make a POST request to the API
  fetch(`/api/vary`, { method: "POST", body: formData })
    .then(async (response) => {
      if (!response.ok) {
        console.log(await response.json());
        throw new Error("Network response was not ok.");
      }
      return response.json();
    })
    .then((data) => {
      // The response data is now available here
      console.log(data);
    })
    .catch((error) => {
      console.error("There was a problem with the fetch operation:", error);
    });
}

// 🎨 Paint (Executes every display frame)
function paint() {}

export const system = "nopaint";
export { boot, paint };

// 📚 Library (Useful functions used throughout the piece)
