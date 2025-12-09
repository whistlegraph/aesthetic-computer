#!/usr/bin/env node
// Filters netlify dev output for cleaner logs
// Passes through stdin to stdout with filtering

import readline from 'readline';

const rl = readline.createInterface({
  input: process.stdin,
  terminal: false
});

// Patterns to skip entirely
const skipPatterns = [
  /^⬥ Rewrote URL to/,
  /^Request from ::ffff:/,
  /^Response with status \d+ in \d+ ms\.$/,
];

// Buffer for potential multi-line compression
let lastLine = '';

rl.on('line', (line) => {
  // Skip lines matching skip patterns
  for (const pattern of skipPatterns) {
    if (pattern.test(line)) {
      return;
    }
  }
  
  // Pass through everything else
  console.log(line);
});

rl.on('close', () => {
  process.exit(0);
});

// Handle SIGINT gracefully
process.on('SIGINT', () => {
  process.exit(0);
});
