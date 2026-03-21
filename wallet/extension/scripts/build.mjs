// Build script for Keeps Wallet extension
// Bundles dependencies and prepares for Chrome loading
// Run: node scripts/build.mjs

import * as esbuild from 'esbuild';
import { existsSync, mkdirSync, copyFileSync, readdirSync, writeFileSync, readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const extDir = join(__dirname, '..');
const distDir = join(extDir, 'dist');

// Ensure dist directory exists
if (!existsSync(distDir)) {
  mkdirSync(distDir, { recursive: true });
}

// Ensure dist/popup directory exists
const distPopupDir = join(distDir, 'popup');
if (!existsSync(distPopupDir)) {
  mkdirSync(distPopupDir, { recursive: true });
}

// Ensure dist/icons directory exists
const distIconsDir = join(distDir, 'icons');
if (!existsSync(distIconsDir)) {
  mkdirSync(distIconsDir, { recursive: true });
}

const isWatch = process.argv.includes('--watch');

async function build() {
  console.log('🔨 Building Keeps Wallet extension...\n');

  // 1. Bundle background.js with dependencies
  console.log('  📦 Bundling background.js...');
  await esbuild.build({
    entryPoints: [join(extDir, 'background.js')],
    bundle: true,
    outfile: join(distDir, 'background.js'),
    format: 'esm',
    platform: 'browser',
    target: 'chrome120',
    minify: !isWatch,
    sourcemap: isWatch,
    external: [], // Bundle everything
  });

  // 2. Bundle inpage.js (injected into pages)
  console.log('  📦 Bundling inpage.js...');
  await esbuild.build({
    entryPoints: [join(extDir, 'inpage.js')],
    bundle: true,
    outfile: join(distDir, 'inpage.js'),
    format: 'iife',
    platform: 'browser',
    target: 'chrome120',
    minify: !isWatch,
    sourcemap: isWatch,
  });

  // 3. Copy content.js (no bundling needed, it's just a relay)
  console.log('  📄 Copying content.js...');
  copyFileSync(join(extDir, 'content.js'), join(distDir, 'content.js'));

  // 4. Copy popup files
  console.log('  📄 Copying popup files...');
  copyFileSync(join(extDir, 'popup', 'popup.html'), join(distPopupDir, 'popup.html'));
  copyFileSync(join(extDir, 'popup', 'popup.js'), join(distPopupDir, 'popup.js'));

  // 4b. Copy Beacon confirmation popup files
  console.log('  📄 Copying confirm.html and confirm.js...');
  copyFileSync(join(extDir, 'confirm.html'), join(distDir, 'confirm.html'));
  copyFileSync(join(extDir, 'confirm.js'), join(distDir, 'confirm.js'));

  // 5. Copy manifest.json
  console.log('  📄 Copying manifest.json...');
  copyFileSync(join(extDir, 'manifest.json'), join(distDir, 'manifest.json'));

  // 6. Copy icons
  console.log('  🎨 Copying icons...');
  const iconsDir = join(extDir, 'icons');
  if (existsSync(iconsDir)) {
    for (const file of readdirSync(iconsDir)) {
      if (file.endsWith('.png')) {
        copyFileSync(join(iconsDir, file), join(distIconsDir, file));
      }
    }
  } else {
    console.log('  ⚠️  No icons found. Run: npm run build:icons');
  }

  console.log('\n✅ Build complete! Extension ready in dist/\n');
  console.log('📋 To install in Chrome:');
  console.log('   1. Open chrome://extensions');
  console.log('   2. Enable "Developer mode" (top right)');
  console.log('   3. Click "Load unpacked"');
  console.log(`   4. Select: ${distDir}`);
  console.log('');
}

if (isWatch) {
  console.log('👀 Watching for changes...\n');
  // Simple watch - rebuild on any change
  // For production, use esbuild's watch API
  build();
} else {
  build().catch((err) => {
    console.error('Build failed:', err);
    process.exit(1);
  });
}
