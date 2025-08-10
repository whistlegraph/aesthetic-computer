#!/usr/bin/env node
/**
 * Simple launcher for the Ableton Live Timeline Viewer
 * 
 * This script automatically finds and runs the viewer with the extracted XML file.
 * Just run: npm run timeline
 */

import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Try to find the XML file in common locations
const possiblePaths = [
  resolve(__dirname, '../system/public/assets/wipppps/zzzZWAP_extracted.xml'),
  '/Users/jas/Desktop/code/aesthetic-computer/system/public/assets/wipppps/zzzZWAP_extracted.xml',
  './zzzZWAP_extracted.xml',
  process.argv[2] // Allow override via command line
].filter(Boolean);

function findXMLFile() {
  for (const path of possiblePaths) {
    if (existsSync(path)) {
      return path;
    }
  }
  return null;
}

function main() {
  console.log('🎵 Starting Ableton Live Timeline Viewer...\n');
  
  const xmlFile = findXMLFile();
  
  if (!xmlFile) {
    console.error('❌ Could not find zzzZWAP_extracted.xml');
    console.log('Tried the following locations:');
    possiblePaths.forEach(path => console.log(`  - ${path}`));
    console.log('\nUsage: npm run timeline [path-to-extracted.xml]');
    process.exit(1);
  }
  
  console.log(`✅ Found XML file: ${xmlFile}\n`);
  
  // Launch the viewer
  const viewer = spawn('node', [resolve(__dirname, 'ableton-live-viewer.mjs'), xmlFile], {
    stdio: 'inherit',
    cwd: __dirname
  });
  
  viewer.on('error', (error) => {
    console.error('❌ Failed to start viewer:', error.message);
    process.exit(1);
  });
  
  viewer.on('exit', (code) => {
    process.exit(code || 0);
  });
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
