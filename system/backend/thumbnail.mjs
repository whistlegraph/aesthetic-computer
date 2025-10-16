#!/usr/bin/env node
// thumbnail.mjs
// Shared thumbnail generator for paintings
// Uses Sharp (like pixel.js) to create 512x512 thumbnails for ATProto

import sharp from "sharp";

/**
 * Generate a thumbnail from an image URL or buffer
 * @param {string|Buffer} source - Image URL or Buffer
 * @param {Object} options - Options
 * @param {number} options.size - Thumbnail size (default: 512)
 * @param {string} options.format - Output format 'png' or 'jpeg' (default: 'png')
 * @param {string} options.fit - How to fit: 'cover', 'contain', 'fill' (default: 'contain')
 * @param {number} options.quality - JPEG quality 1-100 (default: 90)
 * @param {string} options.background - Background color for letterboxing (default: '#000000')
 * @returns {Promise<Buffer>} Thumbnail buffer
 */
export async function generateThumbnail(source, options = {}) {
  const size = options.size || 512;
  const format = options.format || "png";
  const fit = options.fit || "contain"; // 'contain' letterboxes, 'cover' crops
  const quality = options.quality || 90;
  const background = options.background || "#000000"; // Black letterbox

  try {
    let imageBuffer;

    // If source is a URL, fetch it
    if (typeof source === "string") {
      const { got } = await import("got");
      const response = await got(source, {
        responseType: "buffer",
        https: { rejectUnauthorized: process.env.CONTEXT !== "dev" },
      });
      imageBuffer = response.body;
    } else if (Buffer.isBuffer(source)) {
      imageBuffer = source;
    } else {
      throw new Error("Source must be a URL string or Buffer");
    }

    // Get image metadata to check if we need to upscale
    const metadata = await sharp(imageBuffer).metadata();
    const isSmaller = metadata.width < size || metadata.height < size;
    
    // Create thumbnail with Sharp
    let pipeline = sharp(imageBuffer).resize({
      width: size,
      height: size,
      fit, // 'contain' preserves aspect ratio with letterboxing
      kernel: isSmaller ? sharp.kernel.nearest : sharp.kernel.lanczos3, // Nearest neighbor for pixel art upscaling
      background, // Letterbox color
    });

    // Choose output format
    if (format === "jpeg" || format === "jpg") {
      pipeline = pipeline.jpeg({ quality });
    } else {
      pipeline = pipeline.png();
    }

    const thumbnail = await pipeline.toBuffer();
    return thumbnail;
  } catch (error) {
    throw new Error(`Failed to generate thumbnail: ${error.message}`);
  }
}

/**
 * Generate thumbnail from an aesthetic.computer painting slug
 * @param {string} slug - Painting slug (timestamp or short code)
 * @param {string} handleOrCode - User handle (@username) or user code (acXXXXX) - optional, for user paintings
 * @param {Object} options - Options (same as generateThumbnail)
 * @returns {Promise<Buffer>} Thumbnail buffer
 */
export async function getThumbnailFromSlug(slug, handleOrCode = null, options = {}) {
  // Use the /media endpoint which handles authentication and CDN routing
  let imageUrl;
  const cleanSlug = slug.replace(/\.(png|zip)$/i, "");
  const isDev = process.env.CONTEXT === "dev";
  
  if (handleOrCode) {
    // User painting
    if (isDev && options.bucket && options.userId) {
      // Dev mode: bypass /media endpoint and go directly to DO Spaces
      // This avoids SSL certificate issues with localhost fetch calls
      imageUrl = `https://${options.bucket}.sfo3.digitaloceanspaces.com/${options.userId}/painting/${cleanSlug}.png`;
    } else {
      // Production: use /media endpoint which handles both @handle and acXXXXX
      const cleanIdentifier = handleOrCode.replace(/^@/, "");
      imageUrl = `https://aesthetic.computer/media/${cleanIdentifier}/painting/${cleanSlug}.png`;
    }
  } else {
    // Anonymous/guest painting: Direct DO Spaces URL (public bucket)
    imageUrl = `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${cleanSlug}.png`;
  }

  console.log(`🖼️  Fetching: ${imageUrl}`);
  return generateThumbnail(imageUrl, options);
}

/**
 * Get thumbnail stats (size in bytes, dimensions)
 * @param {Buffer} thumbnail - Thumbnail buffer
 * @returns {Promise<Object>} Stats { size, width, height, format }
 */
export async function getThumbnailStats(thumbnail) {
  const metadata = await sharp(thumbnail).metadata();
  return {
    size: thumbnail.length,
    width: metadata.width,
    height: metadata.height,
    format: metadata.format,
  };
}

// CLI usage: node thumbnail.mjs <slug> [handle]
if (import.meta.url === `file://${process.argv[1]}`) {
  const slug = process.argv[2];
  const handle = process.argv[3];

  if (!slug) {
    console.error("Usage: node thumbnail.mjs <slug> [handle]");
    console.error("Examples:");
    console.error("  node thumbnail.mjs 2023.8.24.16.21.09.123 jeffrey");
    console.error("  node thumbnail.mjs Lw2OYs0H");
    process.exit(1);
  }

  try {
    console.log(`\n🎨 Generating thumbnail for: ${slug}`);
    if (handle) console.log(`   Handle: @${handle.replace(/^@/, "")}`);

    const thumbnail = await getThumbnailFromSlug(slug, handle);
    const stats = await getThumbnailStats(thumbnail);

    console.log(`\n✅ Thumbnail generated!`);
    console.log(`   Size: ${(stats.size / 1024).toFixed(2)} KB`);
    console.log(`   Dimensions: ${stats.width}x${stats.height}`);
    console.log(`   Format: ${stats.format}`);

    // Save to file
    const outputFile = `thumbnail-${slug.replace(/[/@:.]/g, "-")}.png`;
    const fs = await import("fs/promises");
    await fs.writeFile(outputFile, thumbnail);
    console.log(`   Saved to: ${outputFile}\n`);
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}\n`);
    process.exit(1);
  }
}
