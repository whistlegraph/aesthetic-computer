#!/usr/bin/env node

import { spawn } from 'child_process';
import { writeFileSync } from 'fs';

// Use Firefox which has better emoji support
const firefoxCommand = `
firefox --headless --screenshot fundraiser-screenshot.png http://localhost:8000/layout-test.html
`;

console.log('Attempting to use Firefox for better emoji rendering...');

// Alternative: Create a simple HTML to PDF converter using ImageMagick
const script = `
#!/bin/bash
# Convert HTML to PDF using different method

# Try with Firefox if available
if command -v firefox &> /dev/null; then
    firefox --headless --print-to-pdf=fundraiser-poster.pdf http://localhost:8000/layout-test.html
    exit 0
fi

# Try with wkhtmltopdf
if command -v wkhtmltopdf &> /dev/null; then
    wkhtmltopdf http://localhost:8000/layout-test.html fundraiser-poster.pdf
    exit 0
fi

# Fallback to chromium with different settings
chromium-browser --headless --no-sandbox --disable-gpu \\
  --disable-software-rasterizer \\
  --print-to-pdf=fundraiser-poster.pdf \\
  --print-to-pdf-no-header \\
  'http://localhost:8000/layout-test.html'
`;

writeFileSync('/tmp/generate-pdf.sh', script, { mode: 0o755 });
console.log('Created PDF generation script');

const proc = spawn('/bin/bash', ['/tmp/generate-pdf.sh'], {
  stdio: 'inherit',
  cwd: process.cwd()
});

proc.on('close', (code) => {
  console.log(`PDF generation completed with code ${code}`);
});
