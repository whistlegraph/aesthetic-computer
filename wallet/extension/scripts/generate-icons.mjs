// Generate extension icons from a simple ꜩ symbol
// Run: node scripts/generate-icons.mjs

import sharp from 'sharp';
import { writeFileSync, mkdirSync, existsSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const iconsDir = join(__dirname, '..', 'icons');

// Ensure icons directory exists
if (!existsSync(iconsDir)) {
  mkdirSync(iconsDir, { recursive: true });
}

// Create an SVG with prompt-inspired pink/iridescent hues
const createIconSvg = (size) => `
<svg width="${size}" height="${size}" viewBox="0 0 ${size} ${size}" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" stop-color="#0b0013"/>
      <stop offset="55%" stop-color="#200028"/>
      <stop offset="100%" stop-color="#ff2fb4"/>
    </linearGradient>
    <radialGradient id="sheen" cx="30%" cy="25%" r="70%">
      <stop offset="0%" stop-color="#ff76ff" stop-opacity="0.6"/>
      <stop offset="60%" stop-color="#ff2fb4" stop-opacity="0.2"/>
      <stop offset="100%" stop-color="#0b0013" stop-opacity="0"/>
    </radialGradient>
    <linearGradient id="glyph" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" stop-color="#7af2ff"/>
      <stop offset="50%" stop-color="#ff76ff"/>
      <stop offset="100%" stop-color="#ff2fb4"/>
    </linearGradient>
    <filter id="glow" x="-50%" y="-50%" width="200%" height="200%">
      <feGaussianBlur stdDeviation="${size * 0.04}" result="blur"/>
      <feMerge>
        <feMergeNode in="blur"/>
        <feMergeNode in="SourceGraphic"/>
      </feMerge>
    </filter>
  </defs>
  <rect width="${size}" height="${size}" rx="${size * 0.18}" fill="url(#bg)"/>
  <rect width="${size}" height="${size}" rx="${size * 0.18}" fill="url(#sheen)"/>
  <g transform="translate(${size * 0.12}, ${size * 0.08}) scale(${size / 80})" fill="url(#glyph)" filter="url(#glow)">
    <!-- Tezos glyph path (converted to outline so the symbol renders reliably) -->
    <path d="M40 2h10v13.5l10-13.5H68L56 18l8 28h-9l-5.8-21.2L40 32v14h9v8H18v-8h9V10H18V2h22z"/>
  </g>
</svg>
`;

const sizes = [16, 32, 48, 128];

async function generateIcons() {
  console.log('🎨 Generating Keeps Wallet icons...');
  
  for (const size of sizes) {
    const svg = createIconSvg(size);
    const outputPath = join(iconsDir, `icon${size}.png`);
    
    await sharp(Buffer.from(svg))
      .png()
      .toFile(outputPath);
    
    console.log(`  ✓ icon${size}.png`);
  }
  
  console.log('✅ Icons generated successfully!');
}

generateIcons().catch(console.error);
