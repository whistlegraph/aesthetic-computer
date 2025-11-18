#!/usr/bin/env node

// Analyze bundle size and find optimization opportunities

import { promises as fs } from 'fs';
import path from 'path';

const bundlePath = process.argv[2] || 'keep-bundles/pie-nft.html';

async function analyze() {
  console.log('📊 Analyzing bundle:', bundlePath);
  console.log('');
  
  const html = await fs.readFile(bundlePath, 'utf8');
  const totalSize = html.length;
  const totalSizeMB = (totalSize / 1024 / 1024).toFixed(2);
  
  console.log('Total file size:', totalSizeMB, 'MB');
  console.log('');
  
  // Find VFS JSON block by counting braces (it's large and nested)
  const vfsStart = html.indexOf('window.VFS = ');
  if (vfsStart === -1) {
    console.log('❌ Could not find VFS data');
    return;
  }

  // Find the matching closing brace
  const jsonStart = vfsStart + 13; // Skip 'window.VFS = '
  let braceCount = 0;
  let inString = false;
  let escaped = false;
  let jsonEnd = jsonStart;

  for (let i = jsonStart; i < html.length; i++) {
    const char = html[i];
    
    if (escaped) {
      escaped = false;
      continue;
    }
    
    if (char === '\\') {
      escaped = true;
      continue;
    }
    
    if (char === '"' && !escaped) {
      inString = !inString;
      continue;
    }
    
    if (!inString) {
      if (char === '{') braceCount++;
      else if (char === '}') {
        braceCount--;
        if (braceCount === 0) {
          jsonEnd = i + 1;
          break;
        }
      }
    }
  }

  const vfsJsonString = html.substring(jsonStart, jsonEnd);
  const vfsSize = vfsJsonString.length;
  const vfsSizeMB = (vfsSize / 1024 / 1024).toFixed(2);
  const vfsPercent = ((vfsSize / totalSize) * 100).toFixed(1);
  
  console.log(`VFS JSON: ${vfsSizeMB} MB (${vfsPercent}%)`);
  
  const vfs = JSON.parse(vfsJsonString);
  const files = Object.keys(vfs);
  console.log('VFS files:', files.length);
  console.log('');
  
  // Analyze by type
  const byType = {};
  let totalBinary = 0;
  let totalText = 0;
  const largestFiles = [];
  
  for (const [filepath, data] of Object.entries(vfs)) {
    const ext = path.extname(filepath).slice(1) || 'none';
    if (!byType[ext]) byType[ext] = { count: 0, size: 0, files: [] };
    byType[ext].count++;
    byType[ext].size += data.content.length;
    byType[ext].files.push({ path: filepath, size: data.content.length });
    
    if (data.binary) totalBinary += data.content.length;
    else totalText += data.content.length;
    
    largestFiles.push({ path: filepath, size: data.content.length, binary: data.binary });
  }
  
  console.log('Binary data (base64):', (totalBinary / 1024 / 1024).toFixed(2), 'MB');
  console.log('Text data:', (totalText / 1024 / 1024).toFixed(2), 'MB');
  console.log('');
  
  console.log('📁 Top 10 file types by size:');
  Object.entries(byType)
    .sort((a, b) => b[1].size - a[1].size)
    .slice(0, 10)
    .forEach(([ext, data]) => {
      const sizeMB = (data.size / 1024 / 1024).toFixed(2);
      const percent = ((data.size / vfsSize) * 100).toFixed(1);
      console.log(`  .${ext.padEnd(8)} ${String(data.count).padStart(4)} files  ${sizeMB.padStart(6)} MB  (${percent.padStart(4)}%)`);
    });
  
  console.log('');
  console.log('📄 Top 20 largest files:');
  largestFiles
    .sort((a, b) => b.size - a.size)
    .slice(0, 20)
    .forEach((file, i) => {
      const sizeKB = (file.size / 1024).toFixed(1);
      const type = file.binary ? '[bin]' : '[txt]';
      console.log(`  ${String(i + 1).padStart(2)}. ${type} ${sizeKB.padStart(7)} KB  ${file.path}`);
    });
  
  console.log('');
  console.log('💡 Optimization opportunities:');
  
  // Check for fonts
  const fontExts = ['woff', 'woff2', 'ttf', 'otf'];
  const fontTypes = Object.entries(byType).filter(([ext]) => fontExts.includes(ext));
  if (fontTypes.length > 0) {
    const fontSize = fontTypes.reduce((sum, [, data]) => sum + data.size, 0);
    const fontSizeMB = (fontSize / 1024 / 1024).toFixed(2);
    const fontPercent = ((fontSize / vfsSize) * 100).toFixed(1);
    console.log(`  🔤 Fonts: ${fontSizeMB} MB (${fontPercent}%) - could use font subsetting or exclude unused fonts`);
  }
  
  // Check for JSON
  if (byType.json) {
    const jsonSizeMB = (byType.json.size / 1024 / 1024).toFixed(2);
    const jsonPercent = ((byType.json.size / vfsSize) * 100).toFixed(1);
    console.log(`  📝 JSON: ${jsonSizeMB} MB (${jsonPercent}%) - could minify or compress glyph data`);
  }
  
  // Check for JS/MJS
  const jsSize = (byType.js?.size || 0) + (byType.mjs?.size || 0);
  if (jsSize > 0) {
    const jsSizeMB = (jsSize / 1024 / 1024).toFixed(2);
    const jsPercent = ((jsSize / vfsSize) * 100).toFixed(1);
    console.log(`  📦 JavaScript: ${jsSizeMB} MB (${jsPercent}%) - could minify/tree-shake unused code`);
  }
  
  // Check for SVG
  if (byType.svg) {
    const svgSizeMB = (byType.svg.size / 1024 / 1024).toFixed(2);
    const svgPercent = ((byType.svg.size / vfsSize) * 100).toFixed(1);
    console.log(`  🖼️  SVG: ${svgSizeMB} MB (${svgPercent}%) - could minify or convert to paths`);
  }
  
  console.log('');
  console.log('🎯 Potential size reduction strategies:');
  console.log('  1. Minify all JS/MJS files (terser)');
  console.log('  2. Tree-shake unused code and dependencies');
  console.log('  3. Subset fonts to only used glyphs');
  console.log('  4. Compress/minify JSON glyph data');
  console.log('  5. Remove unused pieces from disks/');
  console.log('  6. Use gzip/brotli compression at upload time');
  console.log('  7. Convert base64 binary to compressed formats');
}

analyze().catch(console.error);
