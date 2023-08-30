// Pixel, 23.08.26.13.09
// An endpoint to transform / affect input pixels from
// a painting before returning them.

// Usage: /pixel/widthxheight/bucket/painting or add an extension.

// Test URL: https://aesthetic.local:8888/api/pixel/1650x1650/@jeffrey/painting/2023.8.24.16.21.09.123.png
//           https://aesthetic.local:8888/api/pixel/1650x1650/Lw2OYs0H.png

/* #region 🏁 TODO 
  + Done
  - [x] Nearest neighbor scale a painting after opening it.
#endregion */

const { builder } = require("@netlify/functions");
import sharp from "sharp";
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";
const domain = "aesthetic.computer"; // Always use the production assets.

async function handler(event, context) {
  if (
    event.httpMethod === "GET" &&
    (event.headers["host"] === "aesthetic.computer" || dev)
  ) {
    const params = event.path.replace("/api/pixel/", "").split("/");
    const resolution = params[0].split("x").map((n) => parseInt(n));

    const slug = params.slice(1).join("/");
    const imageUrl = `https://${domain}/media/${slug}`;

    console.log(imageUrl);

    if (!imageUrl) {
      return respond(400, { message: "Image URL not provided." });
    }

    try {
      const { got } = await import("got");
      const response = await got(imageUrl, { responseType: "buffer" });

      // Resize the image using nearest neighbor filtering with "sharp"
      // Docs: https://sharp.pixelplumbing.com/api-resize
      const resizedBuffer = await sharp(response.body)
        .resize({
          width: resolution[0],
          height: resolution[1],
          fit: "fill",
          kernel: sharp.kernel.nearest,
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

exports.handler = builder(handler);
