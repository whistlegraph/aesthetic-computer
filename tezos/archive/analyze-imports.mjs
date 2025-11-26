#!/usr/bin/env node

/**
 * Analyze import graph to find which modules are actually used
 * Starting from boot.mjs, trace all imports recursively
 */

import fs from 'fs';
import path from 'path';

const acDir = '/workspaces/aesthetic-computer/system/public/aesthetic.computer';
const visited = new Set();
const imports = new Map(); // module -> list of imports

function extractImports(filePath) {
  const content = fs.readFileSync(filePath, 'utf-8');
  
  // Match all import statements
  const importRegex = /(?:import|export)\s+(?:{[^}]*}|[^'"]*)\s+from\s+["']([^"']+)["']/g;
  const dynamicImportRegex = /import\s*\(["']([^"']+)["']\)/g;
  
  const found = [];
  let match;
  
  while ((match = importRegex.exec(content))) {
    found.push(match[1]);
  }
  
  while ((match = dynamicImportRegex.exec(content))) {
    found.push(match[1]);
  }
  
  return found;
}

function resolveImportPath(basePath, importPath) {
  if (importPath.startsWith('./') || importPath.startsWith('../')) {
    const baseDir = path.dirname(basePath);
    let resolved = path.join(baseDir, importPath);
    
    // Add .mjs extension if needed
    if (!resolved.endsWith('.mjs') && !resolved.endsWith('.js')) {
      if (fs.existsSync(path.join(acDir, resolved + '.mjs'))) {
        resolved += '.mjs';
      } else if (fs.existsSync(path.join(acDir, resolved + '.js'))) {
        resolved += '.js';
      }
    }
    
    return resolved;
  }
  
  return importPath;
}

function analyzeModule(modulePath) {
  if (visited.has(modulePath)) return;
  visited.add(modulePath);
  
  const fullPath = path.join(acDir, modulePath);
  
  if (!fs.existsSync(fullPath)) {
    console.log(`⚠️  Module not found: ${modulePath}`);
    return;
  }
  
  const moduleImports = extractImports(fullPath);
  imports.set(modulePath, moduleImports);
  
  console.log(`📦 ${modulePath} imports:`, moduleImports.length);
  
  // Recursively analyze imports
  moduleImports.forEach(imp => {
    const resolved = resolveImportPath(modulePath, imp);
    analyzeModule(resolved);
  });
}

console.log('🔍 Analyzing import graph from boot.mjs...\n');

analyzeModule('boot.mjs');

console.log(`\n📊 Analysis complete:`);
console.log(`   Total modules used: ${visited.size}`);
console.log(`\n📋 Modules in dependency order:`);

Array.from(visited).forEach((mod, i) => {
  const impCount = imports.get(mod)?.length || 0;
  console.log(`   ${i + 1}. ${mod} (${impCount} imports)`);
});

// Calculate file sizes
console.log(`\n📏 File sizes:`);
let totalSize = 0;
Array.from(visited).forEach(mod => {
  try {
    const fullPath = path.join(acDir, mod);
    const stats = fs.statSync(fullPath);
    totalSize += stats.size;
    const sizeKB = (stats.size / 1024).toFixed(1);
    console.log(`   ${mod}: ${sizeKB} KB`);
  } catch (e) {}
});

console.log(`\n💾 Total unminified size: ${(totalSize / 1024).toFixed(1)} KB`);
console.log(`   Estimated minified: ~${(totalSize / 1024 / 3).toFixed(1)} KB`);
