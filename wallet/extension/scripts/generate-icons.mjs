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

// Create an SVG with the ꜩ symbol on teal background
const createIconSvg = (size) => `
<svg width="${size}" height="${size}" viewBox="0 0 ${size} ${size}" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#0a1628"/>
      <stop offset="100%" style="stop-color:#0d2040"/>
    </linearGradient>
  </defs>
  <rect width="${size}" height="${size}" rx="${size * 0.15}" fill="url(#bg)"/>
  <text 
    x="50%" 
    y="58%" 
    dominant-baseline="middle" 
    text-anchor="middle" 
    font-family="Arial, sans-serif" 
    font-size="${size * 0.6}" 
    font-weight="bold"
    fill="#00b4ff"
  >ꜩ</text>
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
