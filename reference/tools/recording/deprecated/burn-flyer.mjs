#!/usr/bin/env node

// Burn the elcid-flyer at high resolution with MP4 export using Dashboard UI
// Usage: node burn-flyer.mjs

import { spawn } from 'child_process';

console.log('🔥 Burning elcid-flyer with Dashboard UI at 2048x2048 resolution...');

const args = [
  'dashboard-main.mjs',
  'elcid-flyer.mjs',
  '--mp4',
  '--resolution', '2048',
  '--frames', '24'
];

const child = spawn('node', args, {
  stdio: ['inherit', 'pipe', 'pipe'], // Capture stdout and stderr to see summary
  cwd: process.cwd()
});

// Forward output to see the dashboard and summary
child.stdout.on('data', (data) => {
  process.stdout.write(data);
});

child.stderr.on('data', (data) => {
  process.stderr.write(data);
});

child.on('close', (code) => {
  if (code === 0) {
    console.log('✅ Flyer burn complete!');
  } else {
    console.log(`❌ Process exited with code ${code}`);
  }
  process.exit(code);
});

child.on('error', (err) => {
  console.error('❌ Error running burn-flyer:', err);
  process.exit(1);
});