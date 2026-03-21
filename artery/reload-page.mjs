#!/usr/bin/env node
// reload-page.mjs - Reload the page and monitor boot

import CDP from './cdp.mjs';

const cdp = new CDP({ 
  targetUrl: 'https://localhost:8888',
  verbose: true 
});

await cdp.connect();

console.log('🔄 Reloading page...\n');

// Enable console monitoring first
await cdp.send('Runtime.enable');
await cdp.send('Log.enable');
await cdp.send('Page.enable');

let bootMessages = [];

// Listen to console
cdp.ws.on('message', (data) => {
  try {
    const msg = JSON.parse(data);
    
    if (msg.method === 'Runtime.consoleAPICalled') {
      const { type, args } = msg.params;
      const message = args.map(arg => {
        if (arg.value !== undefined) return String(arg.value);
        if (arg.description) return arg.description;
        return `[${arg.type}]`;
      }).join(' ');
      
      bootMessages.push({ type, message, time: Date.now() });
      const icon = type === 'error' ? '❌' : type === 'warn' ? '⚠️' : '📝';
      console.log(`${icon} ${message}`);
    }
    
    if (msg.method === 'Runtime.exceptionThrown') {
      const err = msg.params.exceptionDetails;
      console.log(`💥 ${err.exception?.description || err.text}`);
    }
    
    if (msg.method === 'Page.loadEventFired') {
      console.log('\n✅ Page load event fired');
    }
  } catch (err) {
    // Ignore
  }
});

// Reload
await cdp.send('Page.reload', { ignoreCache: false });

console.log('👂 Monitoring boot process...\n');

// Wait a bit to see what happens
await new Promise(r => setTimeout(r, 10000));

console.log('\n📊 Boot Summary:');
console.log(`  Total messages: ${bootMessages.length}`);
console.log(`  Errors: ${bootMessages.filter(m => m.type === 'error').length}`);
console.log(`  Warnings: ${bootMessages.filter(m => m.type === 'warn').length}`);

// Check if bios loaded
const hasBios = await cdp.eval('typeof window.bios !== "undefined"');
console.log(`  Bios loaded: ${hasBios}`);

cdp.close();
