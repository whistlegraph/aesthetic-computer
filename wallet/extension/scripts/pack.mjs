// Package the built extension into a zip for sideloading.
// Assumes `npm run build` has already produced dist/.

import { existsSync, rmSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '..');
const distDir = join(root, 'dist');
const outZip = join(root, 'keeps-wallet.zip');

if (!existsSync(distDir)) {
  console.error('dist/ not found. Run `npm run build` first.');
  process.exit(1);
}

if (existsSync(outZip)) {
  rmSync(outZip);
}

console.log('📦 Zipping dist/ -> keeps-wallet.zip');
execSync(`cd "${distDir}" && zip -r "${outZip}" .`, { stdio: 'inherit' });
console.log(`✅ Created ${outZip}`);
