// fuzz.mjs - Pixel manipulation piece with basic text rendering
// For recording gifs and mp4s with the orchestrator 

const { floor, random, sin, cos, abs } = Math;

// Simple 8x8 bitmap font for basic characters
const bitmapFont = {
  'A': [
    0b00111000,
    0b01101100,
    0b11000110,
    0b11000110,
    0b11111110,
    0b11000110,
    0b11000110,
    0b00000000
  ],
  'E': [
    0b11111110,
    0b11000000,
    0b11000000,
    0b11111100,
    0b11000000,
    0b11000000,
    0b11111110,
    0b00000000
  ],
  'S': [
    0b01111110,
    0b11000000,
    0b11000000,
    0b01111100,
    0b00000110,
    0b00000110,
    0b11111100,
    0b00000000
  ],
  'T': [
    0b11111110,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00000000
  ],
  'H': [
    0b11000110,
    0b11000110,
    0b11000110,
    0b11111110,
    0b11000110,
    0b11000110,
    0b11000110,
    0b00000000
  ],
  'I': [
    0b01111100,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b01111100,
    0b00000000
  ],
  'C': [
    0b00111100,
    0b01100110,
    0b11000000,
    0b11000000,
    0b11000000,
    0b01100110,
    0b00111100,
    0b00000000
  ],
  ' ': [
    0b00000000,
    0b00000000,
    0b00000000,
    0b00000000,
    0b00000000,
    0b00000000,
    0b00000000,
    0b00000000
  ]
};

let time = 0;
let fuzzAmount = 0.1;

function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { screen } = api;
  time += 0.02;
  
  // Initialize screen on first frame if needed
  if (frameIndex === 0) {
    for (let i = 0; i < screen.pixels.length; i += 4) {
      screen.pixels[i] = 0;     // R
      screen.pixels[i + 1] = 0; // G
      screen.pixels[i + 2] = 0; // B
      screen.pixels[i + 3] = 255; // A
    }
  }
  
  // Clear screen with animated background
  const bgIntensity = floor(20 + sin(time * 0.5) * 10);
  for (let i = 0; i < screen.pixels.length; i += 4) {
    // Add some noise to the background
    const noise = (random() - 0.5) * fuzzAmount * 50;
    screen.pixels[i] = bgIntensity + noise;     // R
    screen.pixels[i + 1] = bgIntensity + noise; // G  
    screen.pixels[i + 2] = bgIntensity + noise; // B
    screen.pixels[i + 3] = 255; // A
  }
  
  // Render "AESTHETIC" text pixel by pixel
  const text = "AESTHETIC";
  const startX = floor(screen.width / 2 - (text.length * 8) / 2);
  const startY = floor(screen.height / 2 - 4);
  
  renderText(screen, text, startX, startY, time);
  
  // Add some pixel fuzz effects around the text
  addFuzzEffects(screen, startX, startY, text.length * 8, 8, time);
}

function renderText(screen, text, x, y, time) {
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    const charData = bitmapFont[char];
    if (!charData) continue;
    
    const charX = x + i * 8;
    
    // Animate character color
    const phase = time + i * 0.3;
    const r = floor(128 + sin(phase) * 127);
    const g = floor(128 + sin(phase + 2) * 127);
    const b = floor(128 + sin(phase + 4) * 127);
    
    // Render each row of the character
    for (let row = 0; row < 8; row++) {
      const rowData = charData[row];
      const pixelY = y + row;
      
      if (pixelY < 0 || pixelY >= screen.height) continue;
      
      // Render each pixel in the row
      for (let col = 0; col < 8; col++) {
        const pixelX = charX + col;
        
        if (pixelX < 0 || pixelX >= screen.width) continue;
        
        // Check if this pixel should be on
        const isPixelOn = (rowData >> (7 - col)) & 1;
        
        if (isPixelOn) {
          const pixelIndex = (pixelY * screen.width + pixelX) * 4;
          
          // Add some fuzz to the text pixels
          const fuzzX = random() * fuzzAmount * 2 - fuzzAmount;
          const fuzzY = random() * fuzzAmount * 2 - fuzzAmount;
          const brightness = 0.8 + random() * 0.4;
          
          screen.pixels[pixelIndex] = floor(r * brightness + fuzzX * 50);     // R
          screen.pixels[pixelIndex + 1] = floor(g * brightness + fuzzY * 50); // G
          screen.pixels[pixelIndex + 2] = floor(b * brightness);              // B
          screen.pixels[pixelIndex + 3] = 255; // A
        }
      }
    }
  }
}

function addFuzzEffects(screen, textX, textY, textWidth, textHeight, time) {
  // Add random pixel glitches around the text
  const numGlitches = floor(50 + sin(time) * 30);
  
  for (let i = 0; i < numGlitches; i++) {
    const glitchX = floor(textX - 20 + random() * (textWidth + 40));
    const glitchY = floor(textY - 20 + random() * (textHeight + 40));
    
    if (glitchX >= 0 && glitchX < screen.width && glitchY >= 0 && glitchY < screen.height) {
      const pixelIndex = (glitchY * screen.width + glitchX) * 4;
      
      // Create colorful glitch pixels
      const intensity = random() * 255;
      const colorChoice = floor(random() * 3);
      
      screen.pixels[pixelIndex + colorChoice] = intensity;
      screen.pixels[pixelIndex + 3] = 255;
    }
  }
  
  // Add scanning lines effect
  const scanLine = floor((sin(time * 10) + 1) * screen.height / 2);
  for (let x = 0; x < screen.width; x++) {
    if (scanLine >= 0 && scanLine < screen.height) {
      const pixelIndex = (scanLine * screen.width + x) * 4;
      screen.pixels[pixelIndex] = 255;     // R
      screen.pixels[pixelIndex + 1] = 255; // G
      screen.pixels[pixelIndex + 2] = 255; // B
      screen.pixels[pixelIndex + 3] = 100; // A - semi-transparent
    }
  }
}

function act({ event, api }) {
  const { screen } = api;
  // Adjust fuzz amount with keyboard
  if (event.key === "ArrowUp") {
    fuzzAmount = Math.min(1.0, fuzzAmount + 0.1);
  }
  if (event.key === "ArrowDown") {
    fuzzAmount = Math.max(0.0, fuzzAmount - 0.1);
  }
}

// Export the piece
export { paint, act };