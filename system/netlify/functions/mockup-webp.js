// mockup-webp, 24.12.19
// Generate an animated WebP with transparency from multiple mockup image URLs
// Usage: /api/mockup-webp?urls=url1,url2,url3,url4&delay=800&size=400
// With QR: /api/mockup-webp?urls=...&qr=mug~CODE~color~via~kidlisp&via=kidlispcode
// Best with 4 frames: right → front → left → front (smooth spin loop)
// Uses Sharp 0.34.3+ pageHeight feature - no gifenc needed!

import { respond } from "../../backend/http.mjs";
import QRCode from "qrcode";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const urlsParam = event.queryStringParameters?.urls;
  const delay = parseInt(event.queryStringParameters?.delay || "800");
  const size = parseInt(event.queryStringParameters?.size || "400");
  const qrSlug = event.queryStringParameters?.qr; // e.g., "mug~CODE~white~via~cow"
  const viaCode = event.queryStringParameters?.via; // KidLisp source code

  if (!urlsParam) {
    return respond(400, { error: "Missing 'urls' parameter" });
  }

  const urls = urlsParam.split(",").map((u) => decodeURIComponent(u.trim()));

  if (urls.length === 0 || urls.length > 8) {
    return respond(400, { error: "Provide 1-8 URLs" });
  }

  try {
    const { default: got } = await import("got");
    const sharp = (await import("sharp")).default;

    console.log("🎨 mockup-webp: Processing", urls.length, "URLs");

    // Generate QR code overlay if slug provided
    let qrOverlay = null;
    if (qrSlug) {
      const qrUrl = `https://aesthetic.computer/${qrSlug}`;
      const qrSize = Math.floor(size * 0.18); // QR is 18% of image size
      const qrBuffer = await QRCode.toBuffer(qrUrl, {
        type: "png",
        width: qrSize,
        margin: 1,
        color: { dark: "#000000", light: "#ffffff" },
        errorCorrectionLevel: "M",
      });
      // Add white background with rounded corners
      qrOverlay = await sharp(qrBuffer)
        .resize(qrSize, qrSize)
        .extend({
          top: 4, bottom: 4, left: 4, right: 4,
          background: { r: 255, g: 255, b: 255, alpha: 230 },
        })
        .toBuffer();
      console.log("🎨 QR overlay generated for:", qrUrl);
    }

    // KidLisp.com label is now on the print file, not the preview animation
    const labelOverlay = null;

    // Fetch and resize all frames to raw RGBA (preserving transparency)
    const frameBuffers = [];

    for (const url of urls) {
      try {
        console.log("🎨 Fetching:", url.substring(0, 80) + "...");
        const response = await got(url, { responseType: "buffer", timeout: { request: 15000 } });
        
        // Start with base image
        let frameImage = sharp(response.body)
          .resize(size, size, {
            fit: "contain",
            background: { r: 0, g: 0, b: 0, alpha: 0 },
            kernel: "lanczos3",
          })
          .ensureAlpha();
        
        // Build composite layers
        const composites = [];
        
        // Add QR code in bottom-left corner
        if (qrOverlay) {
          const qrSize = Math.floor(size * 0.18);
          composites.push({
            input: qrOverlay,
            left: 8,
            top: size - qrSize - 16,
          });
        }
        
        // Add KidLisp label in bottom-right corner (above QR if present)
        if (labelOverlay) {
          const labelInfo = await sharp(labelOverlay).metadata();
          composites.push({
            input: labelOverlay,
            left: size - labelInfo.width - 8,
            top: size - labelInfo.height - 8,
          });
        }
        
        // Apply composites if any
        if (composites.length > 0) {
          frameImage = frameImage.composite(composites);
        }
        
        const rawBuffer = await frameImage.raw().toBuffer();
        frameBuffers.push(rawBuffer);
        console.log("🎨 Frame processed:", size, "x", size);
      } catch (err) {
        console.error("Failed to fetch/process frame:", url, err.message);
      }
    }

    if (frameBuffers.length === 0) {
      return respond(500, { error: "No frames could be processed" });
    }

    // Stack all frame buffers vertically
    const stackedData = Buffer.concat(frameBuffers);
    const totalHeight = size * frameBuffers.length;
    
    console.log("🎨 Stacked", frameBuffers.length, "frames:", size, "x", totalHeight);

    // Create animated WebP directly using Sharp's pageHeight (0.34.3+)
    // This tells Sharp the raw data represents multiple pages/frames
    const webpBuffer = await sharp(stackedData, {
      raw: {
        width: size,
        height: totalHeight,
        channels: 4,
        pageHeight: size,  // Each frame is `size` pixels tall
      },
    })
      .webp({
        quality: 90,
        loop: 0,  // Infinite loop
        delay: Array(frameBuffers.length).fill(delay),  // Same delay for each frame
      })
      .toBuffer();

    console.log(`✅ Generated WebP: ${webpBuffer.length} bytes, ${frameBuffers.length} frames`);

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "image/webp",
        "Cache-Control": "public, max-age=3600",
      },
      body: webpBuffer.toString("base64"),
      isBase64Encoded: true,
    };
  } catch (error) {
    console.error("WebP generation error:", error);
    return respond(500, {
      error: "Failed to generate WebP",
      details: error.message,
    });
  }
}

