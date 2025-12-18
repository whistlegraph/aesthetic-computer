#!/usr/bin/env node
// inspect-prompt-dom.mjs - Inspect what's in the prompt page

import { withCDP } from './cdp.mjs';

await withCDP(async (cdp) => {
  console.log('🔍 Inspecting prompt page DOM...\n');
  
  // Check page state
  const pageInfo = await cdp.eval(`
    ({
      url: location.href,
      title: document.title,
      readyState: document.readyState,
      bodyHTML: document.body?.innerHTML?.substring(0, 500) || 'No body',
      hasBios: typeof window.bios !== 'undefined',
      hasStore: typeof window.store !== 'undefined',
      scripts: Array.from(document.scripts).map(s => s.src || 'inline').slice(0, 5),
      iframes: Array.from(document.querySelectorAll('iframe')).map(f => ({ src: f.src, id: f.id }))
    })
  `);
  
  console.log('📄 Page Info:');
  console.log(`  URL: ${pageInfo.url}`);
  console.log(`  Title: ${pageInfo.title}`);
  console.log(`  Ready State: ${pageInfo.readyState}`);
  console.log(`  Has Bios: ${pageInfo.hasBios}`);
  console.log(`  Has Store: ${pageInfo.hasStore}`);
  
  console.log('\n📜 Scripts:');
  pageInfo.scripts.forEach(s => console.log(`  - ${s}`));
  
  console.log('\n🖼️  Iframes:');
  if (pageInfo.iframes.length > 0) {
    pageInfo.iframes.forEach(f => console.log(`  - ${f.src} (id: ${f.id})`));
  } else {
    console.log('  (none)');
  }
  
  console.log('\n📦 Body HTML preview:');
  console.log(pageInfo.bodyHTML.substring(0, 300));
  
}, { 
  targetUrl: 'https://localhost:8888/prompt',
  verbose: true 
});
