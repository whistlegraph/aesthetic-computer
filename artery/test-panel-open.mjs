#!/usr/bin/env node
// test-panel-open.mjs - Test auto-opening the AC panel

import { createCDP } from './cdp.mjs';

console.log('🧪 Testing AC panel auto-open...\n');

try {
  const cdp = await createCDP({ 
    verbose: true,
    ensurePanel: true 
  });
  
  console.log('\n✅ Successfully connected!');
  
  const info = await cdp.getPageInfo();
  console.log(`\n📄 Page Info:`);
  console.log(`  URL: ${info.url}`);
  console.log(`  Title: ${info.title}`);
  console.log(`  Has Bios: ${info.hasACBios}`);
  
  cdp.close();
} catch (err) {
  console.error(`\n❌ Error: ${err.message}`);
  process.exit(1);
}
