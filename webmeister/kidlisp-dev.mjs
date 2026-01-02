#!/usr/bin/env node
// kidlisp-dev.mjs - Watch console logs from kidlisp.com in Simple Browser

import { Web } from './web.mjs';

const web = new Web({ verbose: true });

async function main() {
  console.log('🔍 Looking for kidlisp.com tab...');
  
  try {
    await web.connect('kidlisp.com');
    console.log('✅ Connected to kidlisp.com\n');
    
    // Enable console logging
    await web.send('Runtime.enable');
    await web.send('Console.enable');
    await web.send('Log.enable');
    
    // Listen for console messages
    web.ws.on('message', data => {
      try {
        const msg = JSON.parse(data);
        
        // Runtime.consoleAPICalled - main console.log etc
        if (msg.method === 'Runtime.consoleAPICalled') {
          const { type, args } = msg.params;
          const values = args.map(a => {
            if (a.type === 'string') return a.value;
            if (a.type === 'number') return a.value;
            if (a.type === 'boolean') return a.value;
            if (a.type === 'undefined') return 'undefined';
            if (a.type === 'object' && a.preview) {
              return JSON.stringify(a.preview.properties?.reduce((o, p) => {
                o[p.name] = p.value;
                return o;
              }, {}) || a.preview);
            }
            return a.description || a.type;
          });
          
          const icon = {
            log: '📝',
            warn: '⚠️',
            error: '❌',
            info: 'ℹ️',
            debug: '🐛',
          }[type] || '📝';
          
          console.log(`${icon} [${type}]`, values.join(' '));
        }
        
        // Runtime.exceptionThrown - uncaught exceptions
        if (msg.method === 'Runtime.exceptionThrown') {
          const { exceptionDetails } = msg.params;
          console.log('💥 [exception]', exceptionDetails.text || exceptionDetails.exception?.description);
        }
        
        // Log.entryAdded - browser/network logs
        if (msg.method === 'Log.entryAdded') {
          const { entry } = msg.params;
          console.log(`🌐 [${entry.level}]`, entry.text, entry.url || '');
        }
        
      } catch (e) { /* ignore parse errors */ }
    });
    
    console.log('📡 Watching console output... (Ctrl+C to stop)\n');
    console.log('─'.repeat(60) + '\n');
    
    // Keep alive
    await new Promise(() => {});
    
  } catch (e) {
    console.error('❌ Error:', e.message);
    console.log('\n💡 Make sure:');
    console.log('   1. CDP tunnel is running: ac-cdp-tunnel');
    console.log('   2. kidlisp.com is open in VS Code Simple Browser');
    process.exit(1);
  }
}

main();
