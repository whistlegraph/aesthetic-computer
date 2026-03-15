#!/usr/bin/env node

// Simple Node.js script to encode kidlisp using the same function as kidlisp.mjs
// Usage: node encode_kidlisp.mjs "your kidlisp code here"

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

// Try to import the actual encodeKidlispForUrl function from kidlisp.mjs
let encodeKidlispForUrl;

try {
  // Attempt to load the actual kidlisp.mjs module
  const __dirname = dirname(fileURLToPath(import.meta.url));
  const kidlispPath = join(__dirname, '../system/public/aesthetic.computer/lib/kidlisp.mjs');
  
  // Since kidlisp.mjs may not export the function, we'll use our own implementation
  // that matches the original exactly, plus handles quotes properly for URLs
  encodeKidlispForUrl = function(source) {
    // First do the basic encoding like kidlisp.mjs
    let encoded = source.replace(/ /g, "_").replace(/\n/g, "~");
    
    // Then encode quotes for URL safety (this is what browsers do automatically)
    encoded = encoded.replace(/"/g, "%22");
    
    return encoded;
  };
} catch (error) {
  // Fallback implementation
  encodeKidlispForUrl = function(source) {
    const encoded = source.replace(/ /g, "_").replace(/\n/g, "~");
    return encoded;
  };
}

// Get the kidlisp code from command line arguments
const args = process.argv.slice(2);
if (args.length === 0) {
  console.error("Usage: node encode_kidlisp.mjs \"kidlisp code\" [baseUrl]");
  process.exit(1);
}

const kidlispCode = args[0];
const baseUrl = args[1] || 'https://localhost:8888';
const encoded = encodeKidlispForUrl(kidlispCode);

// Construct full URL with parameters
const fullUrl = `${baseUrl}/${encoded}?nolabel=true&nogap=true`;
console.log(fullUrl);
