#!/usr/bin/env node
// wait-for-bios.mjs - Wait for bios to load and then interact

import { withCDP } from './cdp.mjs';

await withCDP(async (cdp) => {
  console.log('⏳ Waiting for bios to load...\n');
  
  // Wait for bios
  const startTime = Date.now();
  let attempts = 0;
  const maxAttempts = 50; // 10 seconds max
  
  while (attempts < maxAttempts) {
    const hasBios = await cdp.eval('typeof window.bios !== "undefined"');
    
    if (hasBios) {
      const elapsed = Date.now() - startTime;
      console.log(`✅ Bios loaded after ${elapsed}ms (${attempts} attempts)\n`);
      break;
    }
    
    attempts++;
    await new Promise(r => setTimeout(r, 200));
    
    if (attempts % 5 === 0) {
      process.stdout.write('.');
    }
  }
  
  if (attempts >= maxAttempts) {
    console.log('\n❌ Timeout waiting for bios');
    process.exit(1);
  }
  
  // Now check what we have access to
  const biosInfo = await cdp.eval(`
    ({
      hasBios: typeof window.bios !== 'undefined',
      hasStore: typeof window.store !== 'undefined',
      hasNoPaint: typeof window.nopaint !== 'undefined',
      currentPiece: window.location.pathname,
      canvasCount: document.querySelectorAll('canvas').length,
      bootComplete: window.bootComplete || false
    })
  `);
  
  console.log('🎨 Bios State:');
  Object.entries(biosInfo).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });
  
  // Try to get prompt-specific info
  console.log('\n📝 Prompt Info:');
  const promptInfo = await cdp.eval(`
    ({
      typeBuffer: window.store?.prompt?.typeBuffer || 'N/A',
      cursorPosition: window.store?.prompt?.cursor || 'N/A',
      hasTypeBuffer: typeof window.store?.prompt?.typeBuffer !== 'undefined'
    })
  `);
  
  Object.entries(promptInfo).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });
  
  // Try to send a command by manipulating the store
  if (promptInfo.hasTypeBuffer) {
    console.log('\n💬 Sending text to prompt...');
    await cdp.eval(`
      if (window.store && window.store.prompt) {
        window.store.prompt.typeBuffer = 'hello from cdp! 👋';
        window.store.prompt.cursor = window.store.prompt.typeBuffer.length;
      }
    `);
    
    const newBuffer = await cdp.eval('window.store?.prompt?.typeBuffer || "failed"');
    console.log(`✅ Type buffer set to: "${newBuffer}"`);
  } else {
    console.log('\n⚠️  Type buffer not accessible yet');
  }
  
}, { 
  targetUrl: 'https://localhost:8888/prompt',
  verbose: true 
});
