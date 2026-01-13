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
import QRCode from "qrcode";
import { readFileSync, mkdirSync, copyFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { execSync } from "child_process";
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";

// Load Comic Relief Bold font and register with fontconfig
let comicReliefFontAvailable = false;
try {
  const __dirname = dirname(fileURLToPath(import.meta.url));
  const fontSrc = join(__dirname, "../fonts/ComicRelief-Bold.ttf");
  const fontDir = "/tmp/fonts";
  const fontDest = join(fontDir, "ComicRelief-Bold.ttf");
  
  if (!existsSync(fontDest)) {
    mkdirSync(fontDir, { recursive: true });
    copyFileSync(fontSrc, fontDest);
    // Update fontconfig cache
    execSync(`fc-cache -f ${fontDir} 2>/dev/null || true`);
    console.log("✅ Comic Relief font registered with fontconfig");
  }
  comicReliefFontAvailable = true;
} catch (e) {
  console.warn("⚠️ Comic Relief font setup failed:", e.message);
}

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
    let slug = params.slice(1).join("/");
    let imageUrl; // Declare imageUrl at function scope
    
    // Check for QR code parameter (for print files that need QR baked in)
    // Usage: /api/pixel/2700x1050:contain-clear/CODE.png?qr=mug~+CODE&via=kidlispcode
    const qrSlug = event.queryStringParameters?.qr;
    const qrPosition = event.queryStringParameters?.qrpos || "bottom-right"; // bottom-right, bottom-left, top-right, top-left
    const viaCode = event.queryStringParameters?.via; // KidLisp source code for rotated side label
    
    // Check for + prefix (print/product code) - e.g., "+abc123.png" or "+abc123"
    // Print codes use + prefix like # for paintings, $ for kidlisp, ! for tapes
    const printCodePattern = /^\+([a-z0-9]{6})(\.png|\.webp)?$/;
    const printCodeMatch = slug.match(printCodePattern);
    
    if (printCodeMatch) {
      const productCode = printCodeMatch[1];
      console.log(`🖨️ Looking up print/product by code: +${productCode}`);
      
      try {
        const { getProduct } = await import("../../backend/products.mjs");
        const product = await getProduct(productCode);
        
        if (product?.preview) {
          imageUrl = product.preview;
          console.log(`✅ Resolved +${productCode} to preview: ${imageUrl}`);
        } else {
          // Try direct S3 path for products
          imageUrl = `https://art.aesthetic.computer/products/${productCode}.webp`;
          console.log(`📦 No preview cached, trying S3: ${imageUrl}`);
        }
      } catch (error) {
        console.error(`❌ Error looking up product code: ${error.message}`);
        imageUrl = `https://art.aesthetic.computer/products/${productCode}.webp`;
      }
    }
    
    // Check if slug is a painting code (short alphanumeric without path separators)
    // Examples: "mgy.png", "abc", "t84.png", "8BzEwZGt.png"
    const codePattern = /^([a-zA-Z0-9]{2,10})(\.png)?$/;
    const codeMatch = !imageUrl && slug.match(codePattern);
    
    if (codeMatch) {
      const code = codeMatch[1]; // Extract code without extension
      console.log(`🔍 Looking up painting by code: ${code}`);
      
      try {
        const { connect } = await import("../../backend/database.mjs");
        const database = await connect();
        const painting = await database.db.collection('paintings').findOne({ code });
        
        if (painting) {
          // Build slug from painting data
          // Handle combined slugs (split to get image slug only)
          let imageSlug = painting.slug;
          if (imageSlug.includes(':')) {
            [imageSlug] = imageSlug.split(':');
          }
          
          // Construct DO Spaces URL directly (same logic as media.js edge function)
          const bucket = painting.user 
            ? "user-aesthetic-computer" 
            : "art-aesthetic-computer";
          const key = painting.user 
            ? `${encodeURIComponent(painting.user)}/painting/${imageSlug}.png`
            : `${imageSlug}.png`;
          imageUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`;
          console.log(`✅ Resolved code ${code} to DO Spaces URL: ${imageUrl}`);
        } else {
          // No DB record - try guest art bucket directly (anonymous uploads)
          imageUrl = `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${code}.png`;
          console.log(`📦 No DB record for ${code}, trying guest bucket: ${imageUrl}`);
        }
        
        await database.disconnect();
      } catch (error) {
        console.error(`❌ Error looking up painting code: ${error.message}`);
        // Fall back to guest bucket on DB error too
        imageUrl = `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${code}.png`;
        console.log(`📦 DB error, trying guest bucket: ${imageUrl}`);
      }
    }
    
    // Fall back to constructing URL from slug if not set by code lookup
    if (!imageUrl) {
      imageUrl = `https://${event.headers["host"]}/media/${slug}`;
    }

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

        let resizedImage = sharp(clear ? await original.toBuffer() : combinedImage)
          .resize({
            ...resolution,
            fit: mode,
            kernel,
            background: { r: 0, g: 0, b: 0, alpha: 0 },
          });
        
        // 🔲 Add QR code and KidLisp.com label to print file
        // Position them at the edges of the CONTENT, not the transparent area
        if (qrSlug || viaCode) {
          const composites = [];
          
          // First, get the resized image and find content bounds
          const resizedBuffer = await resizedImage.png().toBuffer();
          const resizedMeta = await sharp(resizedBuffer).metadata();
          
          // For mug wraps (wide images), content is centered horizontally
          // Calculate where the actual artwork sits within the transparent area
          const aspectRatio = md.width / md.height;
          const targetAspect = width / height;
          let contentWidth, contentHeight, contentLeft, contentTop;
          
          if (aspectRatio > targetAspect) {
            // Image is wider - fits width, has vertical padding
            contentWidth = width;
            contentHeight = Math.floor(width / aspectRatio);
            contentLeft = 0;
            contentTop = Math.floor((height - contentHeight) / 2);
          } else {
            // Image is taller - fits height, has horizontal padding (common for mug)
            contentHeight = height;
            contentWidth = Math.floor(height * aspectRatio);
            contentLeft = Math.floor((width - contentWidth) / 2);
            contentTop = 0;
          }
          
          console.log(`📐 Content bounds: ${contentWidth}x${contentHeight} at (${contentLeft}, ${contentTop})`);
          
          // QR code size based on content height (12% - smaller)
          const qrSize = Math.floor(contentHeight * 0.15); // Bigger QR
          const labelMargin = Math.floor(contentHeight * 0.02);
          
          // Generate QR code - position at LEFT side, BELOW the URL label
          if (qrSlug) {
            const qrUrl = `https://aesthetic.computer/${qrSlug}`;
            
            console.log(`🔲 Adding QR code: ${qrUrl} (${qrSize}px)`);
            
            try {
              const qrBuffer = await QRCode.toBuffer(qrUrl, {
                type: "png",
                width: qrSize,
                margin: 1,
                color: { dark: "#000000", light: "#ffffff" },
                errorCorrectionLevel: "H",
              });
              
              // Add white border then black border (no shadow)
              const whiteBorder = 4;
              const blackBorder = 2;
              
              // Create QR with white border
              const whiteBorderedQr = await sharp(qrBuffer)
                .extend({
                  top: whiteBorder, bottom: whiteBorder, left: whiteBorder, right: whiteBorder,
                  background: { r: 255, g: 255, b: 255, alpha: 255 },
                })
                .png()
                .toBuffer();
              
              // Add black border around that
              const paddedQr = await sharp(whiteBorderedQr)
                .extend({
                  top: blackBorder, bottom: blackBorder, left: blackBorder, right: blackBorder,
                  background: { r: 0, g: 0, b: 0, alpha: 255 },
                })
                .png()
                .toBuffer();
              
              const qrMeta = await sharp(paddedQr).metadata();
              // Position: float RIGHT of content with gap, bottom aligned exactly with content
              const qrGap = 15; // Gap between QR and content
              const qrLeft = contentLeft + contentWidth + qrGap; // Right side
              const qrTop = contentTop + contentHeight - qrMeta.height; // Bottom aligned exactly
              
              composites.push({ input: paddedQr, left: Math.max(0, qrLeft), top: qrTop });
              console.log(`✅ QR code outside content edge: (${qrLeft}, ${qrTop})`);
            } catch (qrError) {
              console.error("❌ QR code generation failed:", qrError.message);
            }
          }
          
          // Generate KidLisp.com label with proper colors (rotated 90° on left side of content)
          if (viaCode) {
            // KidLisp letter colors from kidlisp.com homepage
            const letterColors = {
              'K': '#FF6B6B', 'd': '#FFE66D', 'L': '#95E1D3',
              's': '#AA96DA', 'p': '#70D6FF',
              '.': '#95E1D3', // Green dot
              'c': '#FF6B6B', 'o': '#9370DB', 'm': '#90EE90', // com matches play/stop/delete buttons
              '$': '#FFE66D', // Dollar sign in yellow
              '/': '#888888', // Slash in grey
            };
            
            const labelText = `KidLisp.com/$${viaCode}`;
            const fontSize = Math.floor(contentHeight * 0.055);
            
            // Calculate actual text length needed
            // After -90° rotation: text runs bottom-to-top
            // SVG width = thickness of text (font height)
            // SVG height = length of text string
            const textLength = Math.ceil(labelText.length * fontSize * 0.6);
            const labelHeight = Math.ceil(fontSize * 1.5); // SVG width (horizontal) - font thickness
            const labelWidth = textLength + fontSize; // SVG height (vertical) - text length + padding
            
            console.log(`🏷️ Adding KidLisp label: ${labelText} (${fontSize}px)`);
            
            try {
              // Build individual tspan elements for each character with its color
              let tspans = '';
              let blackTspans = ''; // For shadow
              let iCount = 0; // Track which 'i' we're on in KidLisp
              const dollarIndex = labelText.indexOf('$');
              
              for (let i = 0; i < labelText.length; i++) {
                const char = labelText[i];
                let color;
                
                if (i === dollarIndex) {
                  // $ is yellow
                  color = '#FFE66D';
                } else if (i > dollarIndex) {
                  // Code characters after $ are all green
                  color = '#95E1D3';
                } else if (char === 'i') {
                  // First 'i' in KidLisp is teal, second is pink
                  iCount++;
                  color = iCount === 1 ? '#4ECDC4' : '#F38181';
                } else {
                  color = letterColors[char] || '#70D6FF';
                }
                
                // Escape special chars for SVG
                const safeChar = char === '<' ? '&lt;' : char === '>' ? '&gt;' : char === '&' ? '&amp;' : char;
                tspans += `<tspan fill="${color}">${safeChar}</tspan>`;
                blackTspans += `<tspan fill="#000">${safeChar}</tspan>`;
              }
              
              // Create rotated label SVG 
              // Comic Relief is now installed system-wide for librsvg
              const fontFamily = "'Comic Relief', 'Comic Sans MS', cursive, sans-serif";
              
              // After -90° rotation with text-anchor="start":
              // - Anchor point (x,y) is where the FIRST character sits
              // - Text extends UPWARD from there (toward y=0)
              // Position anchor near bottom so text extends upward into SVG
              const textX = labelHeight / 2;
              const textY = labelWidth - fontSize * 0.3; // Near bottom, text extends up
              
              const labelSvg = `
                <svg width="${labelHeight}" height="${labelWidth}" xmlns="http://www.w3.org/2000/svg">
                  <!-- Shadow layer - solid black offset down-right in rotated space -->
                  <text x="${textX + 2}" y="${textY - 2}" 
                        font-family="${fontFamily}" 
                        font-size="${fontSize}" font-weight="bold"
                        text-anchor="start" dominant-baseline="middle"
                        letter-spacing="0.02em"
                        transform="rotate(-90, ${textX + 2}, ${textY - 2})">${blackTspans}</text>
                  <!-- Main colored text -->
                  <text x="${textX}" y="${textY}" 
                        font-family="${fontFamily}" 
                        font-size="${fontSize}" font-weight="bold"
                        text-anchor="start" dominant-baseline="middle"
                        letter-spacing="0.02em"
                        transform="rotate(-90, ${textX}, ${textY})">${tspans}</text>
                </svg>
              `;
              const labelBuffer = await sharp(Buffer.from(labelSvg)).png().toBuffer();
              const labelMeta = await sharp(labelBuffer).metadata();
              
              // Position: LEFT side of content, bottom aligned with content (like QR on right)
              const labelGap = 8; // Further from content
              const labelLeft = contentLeft - labelMeta.width - labelGap; // Left side of content
              // Bottom align with content bottom, nudge down a bit
              const labelTop = contentTop + contentHeight - labelMeta.height + 12;
              
              composites.push({ input: labelBuffer, left: Math.max(0, labelLeft), top: labelTop });
              console.log(`✅ KidLisp label outside content edge: (${labelLeft}, ${labelTop})`);
            } catch (labelError) {
              console.error("❌ KidLisp label generation failed:", labelError.message);
            }
          }
          
          // Apply all composites
          if (composites.length > 0) {
            const resizedBuffer = await resizedImage.png().toBuffer();
            buffer = await sharp(resizedBuffer)
              .composite(composites)
              .png()
              .toBuffer();
          } else {
            buffer = await resizedImage.png().toBuffer();
          }
        } else {
          buffer = await resizedImage.png().toBuffer();
        }
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

export const handler = fun;
