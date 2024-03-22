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
  - [x] Add a compositor for stickers that would be faster than Printful.
  - [x] Add a fitMode.
  - [x] Nearest neighbor scale a painting after opening it.
#endregion */

const { builder } = require("@netlify/functions");
import sharp from "sharp";
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";

async function fun(event, context) {
  if (
    event.httpMethod === "GET" &&
    (event.headers["host"] === "aesthetic.computer" || dev)
  ) {
    const params = event.path.replace("/api/pixel/", "").split("/");
    let [pre, premode] = params[0].split(":");
    premode ||= "fill"; // or "contain", or "conform" or "sticker"
    let [mode, compose] = premode.split("-");
    const clear = compose === "clear";
    // TODO: Eventually use a  "-clear" option to keep the backdrop transparent.
    //       23.09.06.02.27
    const resolution = pre.split("x").map((n) => parseInt(n));
    const slug = params.slice(1).join("/");
    const imageUrl = `https://${event.headers["host"]}/media/${slug}`;

    if (!imageUrl) return respond(400, { message: "Image URL not provided." });

    try {
      const { got } = await import("got");
      const response = await got(imageUrl, {
        responseType: "buffer",
        https: {
          rejectUnauthorized: !dev,
        },
      });

      // Resize the image using nearest neighbor filtering with "sharp"
      // Docs: https://sharp.pixelplumbing.com/api-resize
      let buffer;

      const original = await sharp(response.body);
      const md = await original.metadata();
      console.log("Resolution:", resolution, "Image:", imageUrl);
      console.log("Metadata:", md);
      const width = resolution[0];
      const height = resolution[1] || resolution[0];
      const long = Math.max(md.width, md.height);
      const kernel =
        width > md.width || height > md.height
          ? sharp.kernel.nearest // For scaling up.
          : sharp.kernel.lanczos3; // For scaling down.

      if (mode !== "sticker") {
        // 🟠. Simple resizing.

        // Make sure the original image has a colored backdrop.
        // TODO: Should this actually be a weird gray, or a gradient?
        //       or maybe it should be random?
        let combinedImage;
        if (!clear) {
          combinedImage = await sharp({
            create: {
              width: md.width,
              height: md.height,
              channels: 4,
              background: { r: 32, g: 32, b: 32, alpha: 1 },
            },
          })
            .composite([{ input: await original.toBuffer() }])
            .png()
            .toBuffer();
        }

        // Use "conform" to resize to the longest aspect.
        let resolution;
        if (mode === "conform") {
          resolution =
            md.width >= md.height ? { width: width } : { height: height };
          mode = "inside";
        } else {
          resolution = { width, height };
        }

        buffer = await sharp(clear ? await original.toBuffer() : combinedImage)
          .resize({
            ...resolution,
            fit: mode,
            kernel,
            background: { r: 0, g: 0, b: 0, alpha: 0 },
          })
          .png()
          .toBuffer();
      } else if (mode === "sticker") {
        // 🟠 Complex sticker mockup.
        const scalingFactor =
          md.width > md.height ? width / md.width : height / md.height;

        const margin = 0.1;
        // const marginPx = 128;//Math.floor(long * scalingFactor * margin);
        const marginPx = Math.floor(long * scalingFactor * margin);
        const rectWidth = Math.floor(md.width * scalingFactor) - marginPx;
        const rectHeight = Math.floor(md.height * scalingFactor) - marginPx;

        let combinedImage;
        if (!clear) {
          combinedImage = await sharp({
            create: {
              width: md.width,
              height: md.height,
              channels: 4,
              background: { r: 32, g: 32, b: 32, alpha: 1 },
            },
          })
            .composite([{ input: await original.toBuffer() }])
            .png()
            .toBuffer();
        }

        const resizedBuffer = await sharp(
          clear ? await original.toBuffer() : combinedImage,
        )
          .resize({
            width: rectWidth, // Adjusting the target dimensions for the padding
            height: rectHeight,
            fit: "fill",
            kernel,
            background: getRandomColor(), // { r: 0, g: 0, b: 0, alpha: 1 },
          })
          .toBuffer();

        const radius = Math.floor(long * scalingFactor * 0.02),
          pad = Math.floor(long * scalingFactor * 0.05);

        const svg = `
          <svg width="${rectWidth + marginPx}" height="${
            rectHeight + marginPx
          }" xmlns="http://www.w3.org/2000/svg">
              <defs>
                <filter id="dropshadow" height="130%">
                  <feGaussianBlur in="SourceAlpha" stdDeviation="3"/>
                  <feOffset dx="2" dy="2" result="offsetblur"/>
                  <feComponentTransfer>
                    <feFuncA type="linear" slope="0.5"/>
                  </feComponentTransfer>
                  <feMerge> 
                    <feMergeNode/> 
                    <feMergeNode in="SourceGraphic"/> 
                  </feMerge>
                </filter>
              </defs>
              <rect x="${marginPx / 2 - pad / 2}" y="${
                marginPx / 2 - pad / 2
              }" width="${rectWidth + pad}" height="${
                rectHeight + pad
              }" rx="${radius}" ry="${radius}" fill="white" filter="url(#dropshadow)" />
          </svg>`;

        const rectangleBuffer = await sharp(Buffer.from(svg)).toBuffer();

        // Composite the resized rectangle to the rounded sheet.
        buffer = await sharp(rectangleBuffer)
          .composite([{ input: resizedBuffer }])
          .png()
          .toBuffer();
      }

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "image/png",
          "Content-Length": buffer.length.toString(),
        },
        body: buffer.toString("base64"),
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

function getRandomColor() {
  return {
    r: Math.floor(Math.random() * 256),
    g: Math.floor(Math.random() * 256),
    b: Math.floor(Math.random() * 256),
    alpha: 1,
  };
}

export const handler = builder(fun);
