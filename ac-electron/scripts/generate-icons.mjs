#!/usr/bin/env node
/**
 * Generate app icons from purple-pals.svg
 * 
 * Creates:
 * - build/icon.png (1024x1024 for macOS/Linux)
 * - build/icon.ico (Windows - multiple sizes embedded)
 * - build/icons/*.png (various sizes for Linux)
 */

import sharp from 'sharp';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.join(__dirname, '..');
const buildDir = path.join(rootDir, 'build');
const iconsDir = path.join(buildDir, 'icons');

// Source SVG - use the one from system/public
const svgPath = path.join(rootDir, '..', 'system', 'public', 'purple-pals.svg');

// Icon sizes needed for various platforms
const sizes = {
  // macOS iconset sizes
  mac: [16, 32, 64, 128, 256, 512, 1024],
  // Windows ico sizes  
  win: [16, 24, 32, 48, 64, 128, 256],
  // Linux sizes
  linux: [16, 24, 32, 48, 64, 128, 256, 512]
};

async function ensureDir(dir) {
  try {
    await fs.mkdir(dir, { recursive: true });
  } catch (e) {
    if (e.code !== 'EEXIST') throw e;
  }
}

async function generateIcons() {
  console.log('🎨 Generating icons from purple-pals.svg...\n');
  
  // Read SVG
  let svgContent;
  try {
    svgContent = await fs.readFile(svgPath, 'utf-8');
  } catch (e) {
    console.error(`❌ Could not read SVG: ${svgPath}`);
    console.error(e.message);
    process.exit(1);
  }
  
  // Ensure directories exist
  await ensureDir(buildDir);
  await ensureDir(iconsDir);
  
  // The SVG has a transparent background, let's add a background color
  // for the app icon (purple to match the pals theme)
  const backgroundColor = '#1a1a2e'; // Dark purple/navy background
  
  // Generate main 1024x1024 icon for macOS
  console.log('📱 Generating macOS icon (1024x1024)...');
  await sharp(Buffer.from(svgContent))
    .resize(1024, 1024, {
      fit: 'contain',
      background: backgroundColor
    })
    .flatten({ background: backgroundColor })
    .png()
    .toFile(path.join(buildDir, 'icon.png'));
  console.log('   ✅ build/icon.png');
  
  // Generate Linux icons at various sizes
  console.log('\n🐧 Generating Linux icons...');
  for (const size of sizes.linux) {
    const filename = `${size}x${size}.png`;
    await sharp(Buffer.from(svgContent))
      .resize(size, size, {
        fit: 'contain',
        background: backgroundColor
      })
      .flatten({ background: backgroundColor })
      .png()
      .toFile(path.join(iconsDir, filename));
    console.log(`   ✅ build/icons/${filename}`);
  }
  
  // Generate Windows ICO (sharp can create ico with png2icons or we use multiple PNGs)
  // electron-builder can use icon.png and convert it, or we can provide icon.ico
  // For now, we'll rely on electron-builder's conversion from PNG
  console.log('\n🪟 Windows icon will be generated from icon.png by electron-builder');
  
  // Also copy 256x256 as a fallback ico source
  await sharp(Buffer.from(svgContent))
    .resize(256, 256, {
      fit: 'contain', 
      background: backgroundColor
    })
    .flatten({ background: backgroundColor })
    .png()
    .toFile(path.join(buildDir, 'icon-256.png'));
  console.log('   ✅ build/icon-256.png (fallback)');
  
  console.log('\n✨ Icon generation complete!\n');
  console.log('Files created:');
  console.log('  - build/icon.png (1024x1024) - main app icon');
  console.log('  - build/icon-256.png (256x256) - Windows fallback');
  console.log('  - build/icons/*.png - Linux icons at various sizes');
}

generateIcons().catch(err => {
  console.error('❌ Error generating icons:', err);
  process.exit(1);
});
