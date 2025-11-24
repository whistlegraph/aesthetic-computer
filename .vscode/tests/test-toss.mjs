#!/usr/bin/env node
/**
 * Automated Toss Test using Artery
 * 
 * Runs the toss piece with random throws through Chrome DevTools Protocol
 */

import Artery from '../artery.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const PINK = '\x1b[95m';
const GREEN = '\x1b[92m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);

async function testToss(duration = 30000) {
  
  try {
    console.log('');
    testLog('Starting Toss automated test');
    console.log('');
    
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await new Promise(resolve => setTimeout(resolve, 500));
    
    // Connect
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    await client.jump('toss');
    testLog('Navigated to toss');
    
    // Wait for page to fully load and reconnect context
    await new Promise(r => setTimeout(r, 2000));
    
    // Reconnect after navigation
    client.close();
    await client.connect();
    testLog('Reconnected after navigation');
    await new Promise(r => setTimeout(r, 500));
    
    // Activate audio context
    await client.activateAudio();
    testLog('Audio context activated');
    
    // Define the toss fuzzing algorithm
    const fuzzScript = `
      (async () => {
        // Keyboard shortcuts for toss strips
        // Each strip has 3 keys: [up, play, down]
        const shortcuts = [
          ['1', 'q', 'a'],
          ['2', 'w', 's'],
          ['3', 'e', 'd'],
          ['4', 'r', 'f'],
          ['5', 't', 'g'],
          ['6', 'y', 'h'],
          ['7', 'u', 'j'],
          ['8', 'i', 'k'],
          ['9', 'o', 'l'],
          ['0', 'p', ';'],
          ['-', '[', "'"],
          ['=', ']', '\\\\']
        ];
        
        async function pressKey(key, holdDuration = 50) {
          document.dispatchEvent(new KeyboardEvent('keydown', {
            key: key,
            code: \`Key\${key.toUpperCase()}\`,
            keyCode: key.charCodeAt(0),
            bubbles: true,
            cancelable: true
          }));
          await new Promise(r => setTimeout(r, holdDuration));
          document.dispatchEvent(new KeyboardEvent('keyup', {
            key: key,
            code: \`Key\${key.toUpperCase()}\`,
            keyCode: key.charCodeAt(0),
            bubbles: true,
            cancelable: true
          }));
        }
        
        let plays = 0;
        const startTime = Date.now();
        const maxDuration = ${duration};
        const numStrips = shortcuts.length;
        
        while (Date.now() - startTime < maxDuration) {
          // Pick a random strip
          const stripIdx = Math.floor(Math.random() * numStrips);
          const [up, play, down] = shortcuts[stripIdx];
          
          // Random action: 50% just play, 25% up then play, 25% down then play
          const action = Math.random();
          
          if (action < 0.25) {
            // Up pitch then play
            await pressKey(up, 30);
            await new Promise(r => setTimeout(r, 20));
          } else if (action < 0.5) {
            // Down pitch then play
            await pressKey(down, 30);
            await new Promise(r => setTimeout(r, 20));
          }
          
          // Play the strip
          const playDuration = 100 + Math.random() * 400; // 100-500ms
          await pressKey(play, playDuration);
          plays++;
          
          // Pause between plays
          await new Promise(r => setTimeout(r, 50 + Math.random() * 150)); // 50-200ms
        }
        
        return plays;
      })()
    `;
    
    testLog(`Running fuzzer for ${duration}ms...`);
    const totalPlays = await client.eval(fuzzScript);
    
    console.log('');
    successLog(`Test completed! Played ${totalPlays} strips`);
    console.log('');
    
    // Return to prompt
    await client.jump('prompt');
    testLog('Returned to prompt');
    await new Promise(r => setTimeout(r, 500));
    
    // Close the AC panel
    await Artery.closePanelStandalone();
    
    client.close();
    process.exit(0);
    
  } catch (error) {
    console.error(`${PINK}💔 Test failed: ${error.message}${RESET}`);
    process.exit(1);
  }
}

// Parse duration from command line (default 30 seconds)
const duration = parseInt(process.argv[2]) || 30000;

testToss(duration);
