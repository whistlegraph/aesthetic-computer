// Vary, 23.05.17.02.07
// Vary an existing image with AI.

/* #region ✅ TODO 
  - [] Upload and receive image from `api/vary`.
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ system }) {
  // Convert the pixels data to a Blob object
  const blob = new Blob([system.painting.pixels], {
    type: "application/octet-stream",
  });

  // Convert the Blob to a base64 string
  const imageBase64 = await blobToBase64(blob);

  // Create an object to hold the image data and other data
  const data = {
    image: imageBase64,
    width: system.painting.width,
    height: system.painting.height
  };

  // Create a FormData object to hold the image blob and other data
  // const formData = new FormData();
  // formData.append("image", blob);
  // formData.append("width", system.painting.width);
  // formData.append("height", system.painting.height);

  const host = DEBUG
    ? "http://localhost:3000"
    : "https://ai.aesthetic.computer";

  // Make a POST request to the API
  fetch(`${host}/api/vary`, {
    method: "POST",
    body: JSON.stringify(data),
  })
    .then((response) => {
      if (!response.ok) {
        throw new Error("Network response was not ok");
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
function blobToBase64(blob) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onloadend = () => resolve(reader.result);
    reader.onerror = reject;
    reader.readAsDataURL(blob);
  });
}