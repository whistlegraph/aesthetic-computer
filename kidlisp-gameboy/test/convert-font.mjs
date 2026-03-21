#!/usr/bin/env node
// Convert font_1 JSON glyphs to GameBoy assembly tile data

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fontDir = '/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/drawings/font_1';

// Character set we want to export
const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !?.,';

function rasterizeGlyph(glyphData) {
  const [width, height] = glyphData.resolution;
  const pixels = Array(height).fill().map(() => Array(width).fill(0));
  
  // Draw lines and points
  for (const cmd of glyphData.commands) {
    if (cmd.name === 'line' && cmd.args.length >= 4) {
      const [x1, y1, x2, y2] = cmd.args;
      drawLine(pixels, x1, y1, x2, y2);
    } else if (cmd.name === 'point' && cmd.args.length >= 2) {
      const [x, y] = cmd.args;
      if (y >= 0 && y < height && x >= 0 && x < width) {
        pixels[y][x] = 1;
      }
    }
  }
  
  return pixels;
}

function drawLine(pixels, x1, y1, x2, y2) {
  // Bresenham's line algorithm
  const dx = Math.abs(x2 - x1);
  const dy = Math.abs(y2 - y1);
  const sx = x1 < x2 ? 1 : -1;
  const sy = y1 < y2 ? 1 : -1;
  let err = dx - dy;
  
  let x = x1, y = y1;
  const height = pixels.length;
  const width = pixels[0].length;
  
  while (true) {
    if (y >= 0 && y < height && x >= 0 && x < width) {
      pixels[y][x] = 1;
    }
    
    if (x === x2 && y === y2) break;
    
    const e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x += sx;
    }
    if (e2 < dx) {
      err += dx;
      y += sy;
    }
  }
}

function pixelsTo2bpp(pixels) {
  // Convert 6x10 pixels to 8x16 GameBoy tile (2bpp format)
  // Pad to 8 pixels wide, extend to 16 rows (8 rows per tile in 2 tiles)
  const result = [];
  
  for (let row = 0; row < 16; row++) {
    let low = 0, high = 0;
    
    for (let col = 0; col < 8; col++) {
      const bit = 7 - col; // MSB first
      const pixel = (row < pixels.length && col < pixels[row].length) ? pixels[row][col] : 0;
      
      // For pixel value 1, set both bits (color 11 = darkest)
      if (pixel) {
        low |= (1 << bit);
        high |= (1 << bit);
      }
    }
    
    result.push(low, high);
  }
  
  return result;
}

function byteToHex(byte) {
  return '$' + byte.toString(16).toUpperCase().padStart(2, '0');
}

// Main conversion
const charMap = {
  ' ': 'symbols/space',
  '!': 'symbols/exclamation',
  '?': 'symbols/question mark',
  '.': 'symbols/period',
  ',': 'symbols/comma',
};

// Build character-to-file mapping
for (let i = 0; i < 26; i++) {
  const char = String.fromCharCode(65 + i); // A-Z
  charMap[char] = 'uppercase';
}

for (let i = 0; i < 10; i++) {
  charMap[i.toString()] = 'numbers';
}

console.log('; Font tiles generated from font_1 JSON glyphs');
console.log('; 6x10 glyphs padded to 8x16 GameBoy tiles\n');
console.log('FontTiles:');

let tileIndex = 1;
for (const char of chars) {
  const folder = charMap[char];
  if (!folder) {
    console.log(`; Tile ${tileIndex} - '${char}' (not found)`);
    // Output blank tile
    for (let i = 0; i < 16; i++) {
      console.log('    DB $00, $00');
    }
    tileIndex++;
    continue;
  }
  
  // Find the JSON file
  const folderPath = path.join(fontDir, folder);
  let jsonFile = null;
  
  try {
    const files = fs.readdirSync(folderPath);
    // Look for file starting with the character
    jsonFile = files.find(f => f.startsWith(char + ' ') && f.endsWith('.json'));
  } catch (e) {
    console.log(`; Tile ${tileIndex} - '${char}' (folder not found: ${folder})`);
    for (let i = 0; i < 16; i++) {
      console.log('    DB $00, $00');
    }
    tileIndex++;
    continue;
  }
  
  if (!jsonFile) {
    console.log(`; Tile ${tileIndex} - '${char}' (no JSON)`);
    for (let i = 0; i < 16; i++) {
      console.log('    DB $00, $00');
    }
    tileIndex++;
    continue;
  }
  
  const jsonPath = path.join(folderPath, jsonFile);
  const glyphData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
  
  const pixels = rasterizeGlyph(glyphData);
  const bytes = pixelsTo2bpp(pixels);
  
  console.log(`    ; Tile ${tileIndex} - '${char}'`);
  for (let i = 0; i < bytes.length; i += 2) {
    console.log(`    DB ${byteToHex(bytes[i])}, ${byteToHex(bytes[i+1])}`);
  }
  
  tileIndex++;
}

console.log('FontTilesEnd:');
