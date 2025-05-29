// bdf-glyph, 25.05.27.04.25
// Loads a font file from the aethetic.computer assets directory and adds parsed
// `json` files there as needed, sending them to the client on-demand.

import fetch from "node-fetch";
import zlib from "zlib";
import { promisify } from "util";
import https from "https"; // Added for custom agent
import fs from "fs";
import path from "path";
import {
  S3Client,
  GetObjectCommand,
  PutObjectCommand,
} from "@aws-sdk/client-s3";
import { shell } from "../../backend/shell.mjs";

const gunzip = promisify(zlib.gunzip);
const dev = process.env.NETLIFY_DEV;

// S3 client for assets storage (production only)
const s3Assets = dev
  ? null
  : new S3Client({
      endpoint: "https://" + process.env.ART_ENDPOINT, // Same as 'assets'
      credentials: {
        accessKeyId: process.env.ART_KEY, // Same for 'assets'
        secretAccessKey: process.env.ART_SECRET, // Same for 'assets'
      },
    });

// Local cache directory for development
const localCacheDir =
  "/workspaces/aesthetic-computer/system/public/assets/type/unifont-16.0.03";

// Helper function to display glyph as ASCII art
function logGlyphAsASCII(
  charToFind,
  charCodeToFind,
  glyphData,
  source = "Unknown",
) {
  shell.log(
    `Character: ${charToFind} Code: ${charCodeToFind} 📐 ${glyphData.resolution[0]}x${glyphData.resolution[1]} From: ${source}`,
  );

  // Create a 2D array to represent the bitmap
  const [width, height] = glyphData.resolution;
  const bitmap = Array(height)
    .fill()
    .map(() => Array(width).fill(" "));

  // Fill the bitmap with block characters based on commands
  for (const command of glyphData.commands) {
    if (command.name === "point" && command.args.length >= 2) {
      const [x, y] = command.args;
      if (x >= 0 && x < width && y >= 0 && y < height) {
        bitmap[y][x] = "█";
      }
    } else if (command.name === "line" && command.args.length >= 4) {
      const [x1, y1, x2, y2] = command.args;
      
      // Draw line using Bresenham's algorithm (simplified for axis-aligned lines)
      if (y1 === y2) {
        // Horizontal line
        const startX = Math.min(x1, x2);
        const endX = Math.max(x1, x2);
        for (let x = startX; x <= endX; x++) {
          if (x >= 0 && x < width && y1 >= 0 && y1 < height) {
            bitmap[y1][x] = "█";
          }
        }
      } else if (x1 === x2) {
        // Vertical line
        const startY = Math.min(y1, y2);
        const endY = Math.max(y1, y2);
        for (let y = startY; y <= endY; y++) {
          if (x1 >= 0 && x1 < width && y >= 0 && y < height) {
            bitmap[y][x1] = "█";
          }
        }
      } else {
        // Diagonal line (basic implementation)
        const dx = Math.abs(x2 - x1);
        const dy = Math.abs(y2 - y1);
        const sx = x1 < x2 ? 1 : -1;
        const sy = y1 < y2 ? 1 : -1;
        let err = dx - dy;
        let x = x1, y = y1;
        
        while (true) {
          if (x >= 0 && x < width && y >= 0 && y < height) {
            bitmap[y][x] = "█";
          }
          
          if (x === x2 && y === y2) break;
          
          const e2 = 2 * err;
          if (e2 > -dy) { err -= dy; x += sx; }
          if (e2 < dx) { err += dx; y += sy; }
        }
      }
    }
  }

  // Find first and last non-empty rows to trim whitespace
  let firstNonEmptyRow = -1;
  let lastNonEmptyRow = -1;

  for (let row = 0; row < height; row++) {
    const rowString = bitmap[row].join("");
    if (rowString.trim() !== "") {
      if (firstNonEmptyRow === -1) firstNonEmptyRow = row;
      lastNonEmptyRow = row;
    }
  }

  // Print the bitmap as ASCII art with colors and padding
  if (firstNonEmptyRow !== -1) {
    // Add at least 1 blue row above
    const emptyRow = " ".repeat(width);
    const paddedEmptyRow = "\x1b[44m" + emptyRow + "\x1b[0m";
    console.log(paddedEmptyRow);

    // Print the actual character rows
    for (let row = firstNonEmptyRow; row <= lastNonEmptyRow; row++) {
      const rowString = bitmap[row].join("");
      // ANSI escape codes: \x1b[33m = yellow foreground, \x1b[44m = blue background, \x1b[0m = reset
      // Apply blue background to entire row, yellow foreground to block characters
      const coloredRow =
        "\x1b[44m" + rowString.replace(/█/g, "\x1b[33m█\x1b[44m") + "\x1b[0m";
      console.log(coloredRow);
    }

    // Add at least 1 blue row below
    console.log(paddedEmptyRow);
  }
}

// Function to optimize point commands into horizontal/vertical line commands where possible
function optimizePointsToLines(pointCommands) {
  if (pointCommands.length === 0) return [];

  // First, deduplicate points to ensure no coordinate appears twice
  const uniquePoints = new Map();
  pointCommands.forEach(cmd => {
    const key = `${cmd.args[0]},${cmd.args[1]}`;
    if (!uniquePoints.has(key)) {
      uniquePoints.set(key, { x: cmd.args[0], y: cmd.args[1] });
    }
  });

  // Convert back to array
  const points = Array.from(uniquePoints.values());
  const usedPoints = new Set();
  const commands = [];

  // Sort points for easier processing
  points.sort((a, b) => a.y !== b.y ? a.y - b.y : a.x - b.x);

  for (let i = 0; i < points.length; i++) {
    if (usedPoints.has(i)) continue;

    const point = points[i];
    let lineEnd = null;
    const linePoints = [i];

    // Check for horizontal line (same Y, consecutive X)
    for (let j = i + 1; j < points.length; j++) {
      if (usedPoints.has(j)) continue;
      const nextPoint = points[j];
      
      // If we've moved to a different row, stop checking for horizontal lines
      if (nextPoint.y !== point.y) break;
      
      // Check if this point continues the horizontal line
      const expectedX = point.x + linePoints.length;
      if (nextPoint.x === expectedX) {
        linePoints.push(j);
        lineEnd = nextPoint;
      } else {
        break;
      }
    }

    // If we found a horizontal line with at least 2 points, use it
    if (linePoints.length >= 2) {
      commands.push({
        name: "line",
        args: [point.x, point.y, lineEnd.x, lineEnd.y]
      });
      linePoints.forEach(idx => usedPoints.add(idx));
      continue;
    }

    // Check for vertical line (same X, consecutive Y)
    lineEnd = null;
    const verticalLinePoints = [i];
    
    for (let j = i + 1; j < points.length; j++) {
      if (usedPoints.has(j)) continue;
      const nextPoint = points[j];
      
      // Check if this point continues the vertical line
      if (nextPoint.x === point.x && nextPoint.y === point.y + verticalLinePoints.length) {
        verticalLinePoints.push(j);
        lineEnd = nextPoint;
      }
    }

    // If we found a vertical line with at least 2 points, use it
    if (verticalLinePoints.length >= 2) {
      commands.push({
        name: "line", 
        args: [point.x, point.y, lineEnd.x, lineEnd.y]
      });
      verticalLinePoints.forEach(idx => usedPoints.add(idx));
      continue;
    }

    // No line found, keep as individual point
    commands.push({
      name: "point",
      args: [point.x, point.y]
    });
    usedPoints.add(i);
  }

  return commands;
}

// Check if cached JSON exists for a character code
async function getCachedGlyph(charCode) {
  const key = `${charCode.toString(16).toUpperCase()}.json`;

  if (dev) {
    // Development mode: check local filesystem
    try {
      const filePath = path.join(localCacheDir, key);
      if (fs.existsSync(filePath)) {
        const jsonString = fs.readFileSync(filePath, "utf-8");
        return {
          data: JSON.parse(jsonString),
          source: `Local filesystem: ${filePath}`,
        };
      }
    } catch (error) {
      shell.log(
        `No local cached glyph found for character code ${charCode}: ${error.message}`,
      );
    }
    return null;
  } else {
    // Production mode: check S3
    try {
      const s3Key = `type/unifont-16.0.03/${key}`;
      const getObjectParams = {
        Bucket: "assets-aesthetic-computer",
        Key: s3Key,
      };

      const command = new GetObjectCommand(getObjectParams);
      const response = await s3Assets.send(command);

      // Convert the stream to string
      const chunks = [];
      for await (const chunk of response.Body) {
        chunks.push(chunk);
      }
      const jsonString = Buffer.concat(chunks).toString("utf-8");

      return {
        data: JSON.parse(jsonString),
        source: `S3: https://assets.aesthetic.computer/type/unifont-16.0.03/${key}`,
      };
    } catch (error) {
      // File doesn't exist or other error - will fall back to BDF parsing
      shell.log(
        `No cached glyph found on S3 for character code ${charCode}: ${error.message}`,
      );
      return null;
    }
  }
}

// Cache parsed glyph data to S3 or local filesystem
async function cacheGlyph(charCode, glyphData) {
  const key = `${charCode.toString(16).toUpperCase()}.json`;

  if (dev) {
    // Development mode: save to local filesystem
    try {
      // Ensure the directory exists
      if (!fs.existsSync(localCacheDir)) {
        fs.mkdirSync(localCacheDir, { recursive: true });
      }

      const filePath = path.join(localCacheDir, key);
      fs.writeFileSync(filePath, JSON.stringify(glyphData), "utf-8");

      shell.log(`Cached glyph data locally for character code ${charCode}`);
    } catch (error) {
      // Log error but don't fail the request
      shell.error(
        `Failed to cache glyph locally for character code ${charCode}:`,
        error.message,
      );
    }
  } else {
    // Production mode: save to S3
    try {
      const s3Key = `type/unifont-16.0.03/${key}`;
      const putObjectParams = {
        Bucket: "assets-aesthetic-computer",
        Key: s3Key,
        Body: JSON.stringify(glyphData),
        ContentType: "application/json",
        ACL: "public-read",
      };

      const command = new PutObjectCommand(putObjectParams);
      await s3Assets.send(command);

      shell.log(`Cached glyph data to S3 for character code ${charCode}`);
    } catch (error) {
      // Log error but don't fail the request
      shell.error(
        `Failed to cache glyph to S3 for character code ${charCode}:`,
        error.message,
      );
    }
  }
}

exports.handler = async (event) => {
  const charParam = event.queryStringParameters.char;
  if (!charParam) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Missing 'char' parameter." }),
      headers: { "Content-Type": "application/json" },
    };
  }

  let charToFind, charCodeToFind;

  // Check if the parameter is a Unicode code point (hex string) or actual character
  if (/^[0-9A-F_]+$/i.test(charParam)) {
    // Handle code point format (e.g., "1F600" or "1F600_200D_1F32B")
    // For now, take the first code point if multiple are provided
    const codePointStr = charParam.split("_")[0];
    charCodeToFind = parseInt(codePointStr, 16);

    // Convert code point back to character for display purposes
    try {
      charToFind = String.fromCodePoint(charCodeToFind);
    } catch (error) {
      return {
        statusCode: 400,
        body: JSON.stringify({
          error: `Invalid Unicode code point: ${codePointStr}`,
        }),
        headers: { "Content-Type": "application/json" },
      };
    }
  } else if (charParam.length === 1) {
    // Handle single character format
    charToFind = charParam;
    charCodeToFind = charParam.charCodeAt(0);
  } else {
    // Handle multi-byte characters (emoji, etc.)
    charToFind = charParam;
    charCodeToFind = charParam.codePointAt(0);
  }

  // Check for cached glyph data first
  const cachedGlyph = await getCachedGlyph(charCodeToFind);
  if (cachedGlyph) {
    // Display ASCII art for cached glyph
    logGlyphAsASCII(
      charToFind,
      charCodeToFind,
      cachedGlyph.data,
      cachedGlyph.source,
    );

    return {
      statusCode: 200,
      body: JSON.stringify(cachedGlyph.data),
      headers: { "Content-Type": "application/json" },
    };
  }

  shell.log(
    `No cached glyph found, parsing BDF for character '${charToFind}' (code ${charCodeToFind})`,
  );

  const assetsBaseUrl = dev
    ? process.env.URL || "http://localhost:8888"
    : "https://assets.aesthetic.computer";

  const bdfFileUrl = dev
    ? `${assetsBaseUrl}/assets/type/unifont-16.0.03.bdf.gz`
    : `${assetsBaseUrl}/type/unifont-16.0.03.bdf.gz`;

  shell.log(`Attempting to fetch BDF from: ${bdfFileUrl}`);

  try {
    let fetchOptions = {};
    if (dev && bdfFileUrl.startsWith("https://localhost")) {
      const agent = new https.Agent({
        rejectUnauthorized: false,
      });
      fetchOptions.agent = agent;
      shell.log(
        `Using custom agent for ${bdfFileUrl} to ignore self-signed certificate in local dev.`,
      );
    }

    const response = await fetch(bdfFileUrl, fetchOptions); // Pass fetchOptions
    if (!response.ok) {
      throw new Error(
        `Failed to fetch BDF file: ${response.statusText} from ${bdfFileUrl}`,
      );
    }
    const compressedData = await response.buffer();
    const bdfText = (await gunzip(compressedData)).toString("utf-8");

    // console.log(
    //  "BDF File fetched and decompressed. First 200 chars:",
    //  bdfText.substring(0, 200),
    // ); // Log start of BDF

    const lines = bdfText.split(/\r?\n/); // More robust line splitting
    let inChar = false;
    let currentCharCode = -1;
    let bbx = null;
    let bitmapLines = [];
    let glyphFound = false;

    for (const line of lines) {
      if (line.startsWith("STARTCHAR")) {
        inChar = true;
        currentCharCode = -1;
        bbx = null;
        bitmapLines = [];
      } else if (inChar && line.startsWith("ENCODING")) {
        currentCharCode = parseInt(line.split(" ")[1]);
        // console.log(`Found ENCODING: ${currentCharCode}`); // Optional: very verbose
        if (currentCharCode === charCodeToFind) {
          shell.log(`Target ENCODING ${charCodeToFind} found for a character.`);
        }
      } else if (inChar && line.startsWith("BBX")) {
        const parts = line.split(" ");
        bbx = {
          width: parseInt(parts[1]),
          height: parseInt(parts[2]),
          xOffset: parseInt(parts[3]),
          yOffset: parseInt(parts[4]),
        };
      } else if (inChar && line.startsWith("BITMAP")) {
        // Next lines are bitmap data until ENDCHAR
        // The actual bitmap lines are collected in the else if block below
      } else if (inChar && line.startsWith("ENDCHAR")) {
        if (currentCharCode === charCodeToFind) {
          shell.log(
            `Processing ENDCHAR for target ${charCodeToFind}. BBX:`,
            bbx,
            `Bitmap lines collected: ${bitmapLines.length}`,
          );
        }
        if (
          currentCharCode === charCodeToFind &&
          bbx &&
          bbx.width > 0 &&
          bbx.height > 0
        ) {
          glyphFound = true;
          shell.log(
            `Glyph ${charToFind} (code ${charCodeToFind}) found and processed. BBX width: ${bbx.width}, height: ${bbx.height}`,
          );
          // Process bitmapLines
          const pointCommands = [];
          const bytesPerRow = Math.ceil(bbx.width / 8);

          for (let r = 0; r < bbx.height; r++) {
            if (r >= bitmapLines.length) break; // Should not happen with valid BDF
            const hexRow = bitmapLines[r].trim();
            // Ensure hexRow is padded to be an even number of characters for Buffer.from
            const paddedHexRow =
              hexRow.length % 2 !== 0 ? hexRow + "0" : hexRow;
            const rowBytes = Buffer.from(paddedHexRow, "hex");

            for (let c = 0; c < bbx.width; c++) {
              const byteIndex = Math.floor(c / 8);
              const bitIndex = c % 8;
              if (byteIndex < rowBytes.length) {
                if ((rowBytes[byteIndex] >> (7 - bitIndex)) & 1) {
                  // BDF y-coordinates are typically from baseline.
                  // For pixel art, usually top-left is (0,0).
                  // The BBX height is the number of rows in the bitmap.
                  // The yOffset is from the baseline.
                  // We'll output points relative to the top-left of the bitmap.
                  pointCommands.push({ name: "point", args: [c, r] });
                }
              }
            }
          }

          // Optimize points into lines where possible
          const commands = optimizePointsToLines(pointCommands);

          // Count command types for detailed logging
          const commandCounts = {
            point: 0,
            line: 0
          };
          
          commands.forEach(cmd => {
            if (commandCounts.hasOwnProperty(cmd.name)) {
              commandCounts[cmd.name]++;
            }
          });

          shell.log(
            `Optimized ${pointCommands.length} points into ${commands.length} commands: ` +
            `${commandCounts.line} lines, ${commandCounts.point} points`
          );

          const glyphData = {
            resolution: [bbx.width, bbx.height],
            commands: commands,
          };

          // Log glyph as ASCII art using block characters
          logGlyphAsASCII(charToFind, charCodeToFind, glyphData, "Generated");

          // Cache the parsed glyph data to S3 for future requests
          await cacheGlyph(charCodeToFind, glyphData);

          return {
            statusCode: 200,
            body: JSON.stringify(glyphData),
            headers: { "Content-Type": "application/json" },
          };
        }
        inChar = false;
      } else if (inChar && bbx && bitmapLines.length < bbx.height) {
        // This line is part of the BITMAP data
        bitmapLines.push(line.trim());
      }
    }

    if (!glyphFound) {
      shell.log(
        `Glyph for character '${charToFind}' (code ${charCodeToFind}) was not found after parsing the entire file.`,
      );
      return {
        statusCode: 404,
        body: JSON.stringify({
          error: `Glyph for character '${charToFind}' (code ${charCodeToFind}) not found.`,
        }),
        headers: { "Content-Type": "application/json" },
      };
    }
  } catch (error) {
    shell.error("Error in bdf-glyph function:", error);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: error.message }),
      headers: { "Content-Type": "application/json" },
    };
  }
};
