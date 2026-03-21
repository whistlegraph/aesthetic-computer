#!/usr/bin/env node
// test-prompt-interaction.mjs - Test interacting with the prompt

import { withCDP } from './cdp.mjs';

await withCDP(async (cdp) => {
  console.log('🧪 Testing prompt interaction...\n');
  
  // Get current input value
  const currentValue = await cdp.eval(`
    document.querySelector('input')?.value || ''
  `);
  console.log(`Current input value: "${currentValue}"`);
  
  // Set new value
  const newValue = 'hello from cdp system! 👋';
  await cdp.eval(`
    const input = document.querySelector('input');
    if (input) {
      input.value = '${newValue}';
      input.focus();
      input.dispatchEvent(new Event('input', { bubbles: true }));
    }
  `);
  
  console.log(`✅ Set input to: "${newValue}"`);
  
  // Verify it was set
  const verifyValue = await cdp.eval(`
    document.querySelector('input')?.value || ''
  `);
  console.log(`Verified value: "${verifyValue}"`);
  
  // Get some info about the prompt state
  const info = await cdp.eval(`
    ({
      hasInput: !!document.querySelector('input'),
      inputPlaceholder: document.querySelector('input')?.placeholder,
      hasBios: typeof window.bios !== 'undefined',
      currentPiece: window.location.pathname
    })
  `);
  
  console.log('\n📊 Prompt state:');
  console.log(`  Has input: ${info.hasInput}`);
  console.log(`  Placeholder: ${info.inputPlaceholder}`);
  console.log(`  Has bios: ${info.hasBios}`);
  console.log(`  Current piece: ${info.currentPiece}`);
  
}, { 
  targetUrl: 'https://localhost:8888/prompt',
  verbose: true 
});
