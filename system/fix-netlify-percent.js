#!/usr/bin/env node
// Fix Netlify CLI to handle literal % symbols in URLs

const fs = require('fs');
const path = require('path');

const filePath = path.join(__dirname, 'node_modules/netlify-cli/dist/utils/rules-proxy.js');

console.log('🔧 Fixing Netlify CLI to handle % symbols...');

try {
  let content = fs.readFileSync(filePath, 'utf8');
  
  // Check if already patched
  if (content.includes('try { return decodeURIComponent')) {
    console.log('✅ Already patched!');
    process.exit(0);
  }
  
  // Create backup
  fs.writeFileSync(filePath + '.backup', content);
  
  // Apply the fix - wrap decodeURIComponent in try-catch
  const original = 'path: decodeURIComponent(reqUrl.pathname),';
  const fixed = `path: (() => {
                try {
                    return decodeURIComponent(reqUrl.pathname);
                } catch (e) {
                    return reqUrl.pathname;
                }
            })(),`;
  
  if (!content.includes(original)) {
    console.log('❌ Could not find the line to patch!');
    process.exit(1);
  }
  
  content = content.replace(original, fixed);
  fs.writeFileSync(filePath, content);
  
  console.log('✅ Patch applied successfully!');
  console.log('📁 Backup saved to:', filePath + '.backup');
  console.log('🔄 Restart your dev server for changes to take effect.');
  
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
