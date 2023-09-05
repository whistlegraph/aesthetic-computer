// Pixel, 23.08.26.13.09
// An endpoint to transform / affect input pixels from
// a painting before returning them.

// Usage: /pixel/widthxheight/bucket/painting or add an extension.

// Test URLs: https://aesthetic.local:8888/api/pixel/1650x1650/@jeffrey/painting/2023.8.24.16.21.09.123.png
//            https://aesthetic.local:8888/api/pixel/1650x1650/Lw2OYs0H.png
//            https://aesthetic.local:8888/api/pixel/1650x1650:contain/@jeffrey/painting/2023.9.04.21.10.34.574.png
//                                                             ^ mode for fit

/* #region 🏁 TODO 
  + Done
  - [x] Add a fitMode.
  - [x] Nearest neighbor scale a painting after opening it.
#endregion */

const { builder } = require("@netlify/functions");
import sharp from "sharp";
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";
const domain = "aesthetic.computer"; // Always use the production assets.

async function fun(event, context) {
  if (
    event.httpMethod === "GET" &&
    (event.headers["host"] === "aesthetic.computer" || dev)
  ) {
    const params = event.path.replace("/api/pixel/", "").split("/");
    let [pre, fit] = params[0].split(":");
    fit ||= "fill"; // or "fill"
    const resolution = pre.split("x").map((n) => parseInt(n));
    const slug = params.slice(1).join("/");
    const imageUrl = `https://${domain}/media/${slug}`;

    console.log("Image URL:", imageUrl);

    if (!imageUrl) return respond(400, { message: "Image URL not provided." });

    try {
      const { got } = await import("got");
      const response = await got(imageUrl, { responseType: "buffer" });

      // Resize the image using nearest neighbor filtering with "sharp"
      // Docs: https://sharp.pixelplumbing.com/api-resize
      const resizedBuffer = await sharp(response.body)
        .resize({
          width: resolution[0],
          height: resolution[1] || resolution[0],
          fit, // or "fill" to stretch it
          kernel: sharp.kernel.nearest,
          background: { r: 0, g: 0, b: 0, alpha: 0 },
        })
        .png()
        .toBuffer();

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "image/png",
          "Content-Length": resizedBuffer.length.toString(),
        },
        body: resizedBuffer.toString("base64"),
        ttl: 60,
        isBase64Encoded: true,
      };
    } catch (error) {
      return respond(500, {
        message: "Internal Server Error",
        error: error.message,
      });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}

export const handler = builder(fun);
