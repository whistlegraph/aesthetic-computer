#!/usr/bin/env node

/**
 * Sixel Graphics Renderer
 * A proper rendering pipeline for sixel graphics with Bresenham line drawing
 */

class SixelRenderer {
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.buffer = new Array(height).fill(null).map(() => new Array(width).fill(0));
    this.colors = new Map();
    this.currentColor = 0;
  }

  // Define a color (RGB values 0-100)
  defineColor(colorId, r, g, b) {
    this.colors.set(colorId, { r, g, b });
  }

  // Set current drawing color
  setColor(colorId) {
    this.currentColor = colorId;
  }

  // Clear the buffer
  clear() {
    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        this.buffer[y][x] = 0;
      }
    }
  }

  // Set a pixel
  setPixel(x, y) {
    if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
      this.buffer[y][x] = this.currentColor;
    }
  }

  // Bresenham line algorithm
  drawLine(x0, y0, x1, y1) {
    const dx = Math.abs(x1 - x0);
    const dy = Math.abs(y1 - y0);
    const sx = x0 < x1 ? 1 : -1;
    const sy = y0 < y1 ? 1 : -1;
    let err = dx - dy;

    let x = x0;
    let y = y0;

    while (true) {
      this.setPixel(x, y);

      if (x === x1 && y === y1) break;

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

  // Draw an X shape
  drawX() {
    // Top-left to bottom-right diagonal
    this.drawLine(0, 0, this.width - 1, this.height - 1);
    // Top-right to bottom-left diagonal
    this.drawLine(this.width - 1, 0, 0, this.height - 1);
  }

  // Convert buffer to sixel format
  toSixel() {
    let sixel = '\x1bP0;0;0q';
    
    // Add raster attributes (width, height)
    sixel += `"1;1;${this.width};${this.height}`;
    
    // Define colors
    for (const [colorId, color] of this.colors) {
      sixel += `#${colorId};2;${color.r};${color.g};${color.b}`;
    }

    // Process buffer in chunks of 6 vertical pixels (sixels)
    const sixelRows = Math.ceil(this.height / 6);
    
    for (let row = 0; row < sixelRows; row++) {
      const yStart = row * 6;
      
      // Group pixels by color for this sixel row
      const colorData = new Map();
      
      for (let x = 0; x < this.width; x++) {
        for (let colorId of this.colors.keys()) {
          if (!colorData.has(colorId)) {
            colorData.set(colorId, []);
          }
          
          let sixelValue = 0;
          // Check 6 vertical pixels for this sixel
          for (let bit = 0; bit < 6; bit++) {
            const y = yStart + bit;
            if (y < this.height && this.buffer[y][x] === colorId) {
              sixelValue |= (1 << bit);
            }
          }
          
          // Convert to sixel character (offset by 63)
          const sixelChar = String.fromCharCode(63 + sixelValue);
          colorData.get(colorId).push(sixelChar);
        }
      }
      
      // Output each color's data for this row
      for (const [colorId, chars] of colorData) {
        if (chars.some(c => c !== '?')) { // Only output if there are visible pixels
          sixel += `#${colorId}`;
          
          // Simple run-length encoding
          let i = 0;
          while (i < chars.length) {
            const char = chars[i];
            let count = 1;
            
            // Count consecutive identical characters
            while (i + count < chars.length && chars[i + count] === char) {
              count++;
            }
            
            if (count > 3) {
              sixel += `!${count}${char}`;
            } else {
              sixel += char.repeat(count);
            }
            
            i += count;
          }
        }
      }
      
      // End of sixel row
      if (row < sixelRows - 1) {
        sixel += '-'; // Graphics new line
      }
    }
    
    // String terminator
    sixel += '\x1b\\';
    
    return sixel;
  }

  // Render and output
  render() {
    return this.toSixel();
  }
}

// Test: Create a red X
function createRedX() {
  const renderer = new SixelRenderer(64, 64);
  
  // Define red color
  renderer.defineColor(1, 100, 0, 0);
  renderer.setColor(1);
  
  // Draw the X
  renderer.drawX();
  
  return renderer.render();
}

// Main execution
if (import.meta.url === `file://${process.argv[1]}`) {
  const sixelData = createRedX();
  process.stdout.write(sixelData);
}

export { SixelRenderer };
