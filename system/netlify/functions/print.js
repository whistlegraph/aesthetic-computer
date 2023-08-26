// Print, 23.08.25.14.37
// Order stickers or get mockup imagery from a POD API (Printful)

// Usage: /print?image=imageurl;
//               ^ TODO: This should be a painting code / url?

// Test URL: https://aesthetic.computer/api/print?pixels=https://aesthetic.computer/api/pixel/1650x1650/art/Lw2OYs0H

/* #region 🏁 TODO 
  - [] Get mockup images working and looking good for different
       resolutions.
       - [] Test a painting that is at a different resolution / try
            a cropped image.
  - [] Create an actual order. (Is it possible to make a fake order / cancel one after making it?)
  + Done
  - [x] Find a library that can decode and rescale the PNG.
  - [x] Should this eventually go in a separate API call?
  - [x] Nearest-neighbor stretch the painting to fit 1650x1650
      - [x] And place it on a URL.
      - https://aesthetic.local:8888/api/pixel/1024x1024/art/Lw2OYs0H
#endregion */

import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";
const printfulKey = process.env.PRINTFUL_API_TOKEN;

export async function handler(event, context) {
  const { got } = await import("got");

  const API = "https://api.printful.com";
  const headers = {
    Authorization: "Bearer " + printfulKey,
    "Content-Type": "application/json",
  };

  if (
    event.httpMethod === "GET" &&
    (event.headers["host"] === "aesthetic.computer" || dev)
  ) {
    try {
      // 🔸 List all products.
      // (Only really needed while developing for now. 23.08.25.18.42)
      // const products = await got.get(`${API_URL}/products?category_id=202`, {
      // headers,
      // });
      // return respond(200, { product: JSON.parse(products.body) });

      // 🔍
      // 202 - Stickers `category_id`.
      // 358 - Kiss-cut sticker product `id`.
      const id = 358;

      // 🔸 Get information for a single product.
      // (Which includes its availailbility status by region, per variant.)
      // TODO: Make sure it's stock before purchasing? 23.08.25.18.50
      // const product = await got.get(`${API}/products/${id}`, { headers });
      // return respond(200, { product: JSON.parse(product.body) });

      // 🔍
      // ??? - Kiss-cut sticker 5.5" square `variant_id`.
      const variant = 10165;

      // 🔸 Get print constraints for a given product.
      // const printfiles = await got.get(
      //   `${API}/mockup-generator/printfiles/${id}`,
      //   { headers }
      // );
      // return respond(200, { product: JSON.parse(printfiles.body) });

      // 🔸 Generate a mockup image.
      const imageUrl = event.queryStringParameters.pixels;

      const task = await got.post(`${API}/mockup-generator/create-task/${id}`, {
        headers: headers,
        json: {
          variant_ids: [variant],
          files: [
            {
              placement: "default", // 185
              image_url: imageUrl,
              position: {
                area_width: 1650,
                area_height: 1650,
                width: 1650,
                height: 1650,
                top: 0,
                left: 0,
              },
            },
          ], // See also: https://developers.printful.com/docs/#operation/createGeneratorTask
          format: "png",
        },
      });

      const taskResult = JSON.parse(task.body)?.result;
      // return respond(200, { task: taskResult }) });

      console.log("Task result:", taskResult);

      // Polling logic
      let mockupUrl = null;
      const maxAttempts = 12;
      let attempt = 0;
      while (attempt < maxAttempts) {
        const statusResponse = await got.get(
          `${API}/mockup-generator/task/?task_key=${taskResult?.task_key}`,
          { headers }
        );
        const statusResult = JSON.parse(statusResponse.body);

        if (statusResult.result.status === "completed") {
          console.log("Result:", statusResult);
          mockupUrl = statusResult.result.mockups[0].mockup_url;
          break;
        } else {
          await new Promise((res) => setTimeout(res, 2000)); // wait 2 seconds.
          attempt += 1;
        }
      }

      if (mockupUrl) {
        return respond(200, { sticker: mockupUrl });
      } else {
        return respond(500, {
          message: "Timed out waiting for mockup generation.",
        });
      }
    } catch (error) {
      console.error("Failed to generate mockup:", error.message);
      return respond(500, { message: error.message });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
