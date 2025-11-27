#!/usr/bin/env node
/**
 * Automated Notepat Test using Artery
 * 
 * Runs the notepat fuzzing algorithm through Chrome DevTools Protocol
 */

import Artery from '../../artery/artery.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const PINK = '\x1b[95m';
const GREEN = '\x1b[92m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);

async function testNotepat(duration = 30000) {
  
  try {
    console.log('');
    testLog('Starting Notepat automated test');
    console.log('');
    
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await new Promise(resolve => setTimeout(resolve, 500));
    
    // Connect
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    await client.jump('notepat');
    testLog('Navigated to notepat');
    
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
    
    // Define the fuzzing algorithm
    const fuzzScript = `
      (async () => {
        const notes = ['c', 'd', 'e', 'f', 'g', 'a', 'b', 'h', 'i', 'j', 'k', 'l', 'm', 'n'];
        const octaves = ['3', '4', '5', '6', '7', '8'];
        
        async function pressKey(key, code, keyCode, holdDuration = 30) {
          document.dispatchEvent(new KeyboardEvent('keydown', {
            key, code, keyCode, bubbles: true, cancelable: true
          }));
          await new Promise(r => setTimeout(r, holdDuration));
          document.dispatchEvent(new KeyboardEvent('keyup', {
            key, code, keyCode, bubbles: true, cancelable: true
          }));
        }
        
        let totalNotes = 0;
        const startTime = Date.now();
        const maxDuration = ${duration};
        
        while (Date.now() - startTime < maxDuration) {
          // Random octave
          const octave = octaves[Math.floor(Math.random() * octaves.length)];
          await pressKey(octave, \`Digit\${octave}\`, 48 + parseInt(octave));
          await new Promise(r => setTimeout(r, 50));
          
          // Maybe change wavetype
          if (Math.random() > 0.5) {
            await pressKey('Tab', 'Tab', 9);
            await new Promise(r => setTimeout(r, 50));
          }
          
          // Play random phrase
          const goingUp = Math.random() > 0.5;
          const scaleNotes = goingUp ? notes : [...notes].reverse();
          const numNotes = 3 + Math.floor(Math.random() * 6);
          const startIdx = Math.floor(Math.random() * (scaleNotes.length - numNotes));
          const notesToPlay = scaleNotes.slice(startIdx, startIdx + numNotes);
          
          for (const note of notesToPlay) {
            const holdDuration = 20 + Math.random() * 40;
            await pressKey(note, \`Key\${note.toUpperCase()}\`, note.charCodeAt(0), holdDuration);
            await new Promise(r => setTimeout(r, 10 + Math.random() * 30));
            totalNotes++;
          }
        }
        
        return totalNotes;
      })()
    `;
    
    testLog(`Running fuzzer for ${duration}ms...`);
    const totalNotes = await client.eval(fuzzScript);
    
    console.log('');
    successLog(`Test completed! Played ${totalNotes} notes`);
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
    client.close();
    process.exit(1);
  }
}

// Parse duration from command line (default 30 seconds)
const duration = parseInt(process.argv[2]) || 30000;

testNotepat(duration);
