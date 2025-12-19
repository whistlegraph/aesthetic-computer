#!/usr/bin/env node
// listen-console.mjs - Listen to console messages from the page

import CDP from './cdp.mjs';

const cdp = new CDP({ 
  targetUrl: 'https://localhost:8888/prompt',
  verbose: true 
});

await cdp.connect();

// Enable console before we enable the handler
await cdp.send('Runtime.enable');
await cdp.send('Log.enable');

console.log('👂 Listening to browser console...\n');
console.log('(Press Ctrl+C to stop)\n');

// Listen to console messages
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
      
      const icon = type === 'error' ? '❌' : type === 'warn' ? '⚠️' : '📝';
      console.log(`${icon} [${type}] ${message}`);
    }
    
    if (msg.method === 'Runtime.exceptionThrown') {
      console.log(`💥 Exception: ${msg.params.exceptionDetails.text}`);
      if (msg.params.exceptionDetails.exception) {
        console.log(`   ${msg.params.exceptionDetails.exception.description}`);
      }
    }
    
    if (msg.method === 'Log.entryAdded') {
      const entry = msg.params.entry;
      console.log(`📋 [${entry.level}] ${entry.text}`);
    }
  } catch (err) {
    // Ignore parse errors
  }
});

// Keep alive
process.on('SIGINT', () => {
  console.log('\n👋 Stopping console listener');
  cdp.close();
  process.exit(0);
});
