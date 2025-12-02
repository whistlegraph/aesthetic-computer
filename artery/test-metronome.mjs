#!/usr/bin/env node
/**
 * 🕐 Metronome Debug Test
 * 
 * Tests the metronome piece to debug why it only ticks once.
 * Monitors console output to see beat and sim function calls.
 * 
 * Usage: node test-metronome.mjs
 */

import Artery from './artery.mjs';

// Terminal colors
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🕐${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);
const errorLog = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);

async function main() {
  testLog('Metronome Debug Test');
  testLog('==================');
  
  const client = new Artery();
  
  try {
    // Connect to the browser
    await client.connect();
    testLog('Connected to browser');
    
    // Enable console logging with custom callback
    let beatCount = 0;
    let simCount = 0;
    let lastBeatTime = null;
    
    await client.enableConsole((type, message) => {
      const timestamp = new Date().toLocaleTimeString();
      
      // Colorize based on type
      if (message.includes('BEAT')) {
        beatCount++;
        const timeSinceLast = lastBeatTime ? `(+${Date.now() - lastBeatTime}ms)` : '';
        lastBeatTime = Date.now();
        console.log(`${CYAN}[${timestamp}] 🎵 BEAT #${beatCount} ${timeSinceLast}${RESET}`);
        console.log(`${DIM}   ${message}${RESET}`);
      } else if (message.includes('SIM')) {
        simCount++;
        if (simCount % 10 === 0) { // Only show every 10th sim log
          console.log(`${DIM}[${timestamp}] 🔄 SIM x${simCount}${RESET}`);
        }
      } else if (message.includes('DAW')) {
        console.log(`${YELLOW}[${timestamp}] 🎛️ ${message}${RESET}`);
      } else if (message.includes('error') || message.includes('Error')) {
        console.log(`${RED}[${timestamp}] ❌ ${message}${RESET}`);
      } else if (type === 'warn') {
        console.log(`${YELLOW}[${timestamp}] ⚠️  ${message}${RESET}`);
      } else {
        console.log(`${DIM}[${timestamp}] ${message}${RESET}`);
      }
    });
    testLog('Console logging enabled');
    
    // Navigate to metronome
    testLog('Jumping to metronome...');
    await client.jump('metronome');
    await new Promise(r => setTimeout(r, 1000));
    testLog('On metronome piece');
    
    // Activate audio by clicking
    testLog('Activating audio (clicking)...');
    await client.click(100, 100);
    await new Promise(r => setTimeout(r, 500));
    
    // Click again to make sure audio is activated
    await client.click(150, 150);
    await new Promise(r => setTimeout(r, 500));
    
    successLog('Audio should be active now');
    testLog('');
    testLog('Monitoring for 10 seconds...');
    testLog('Expected: BEAT messages every ~333ms (at 180 BPM)');
    testLog('');
    
    // Wait and monitor
    await new Promise(r => setTimeout(r, 10000));
    
    // Summary
    testLog('');
    testLog('=== SUMMARY ===');
    if (beatCount === 0) {
      errorLog('No BEAT messages received! Audio context may not be running.');
    } else if (beatCount === 1) {
      errorLog(`Only 1 BEAT received. Beat loop is not continuing.`);
    } else {
      successLog(`Received ${beatCount} BEATs in 10 seconds`);
      const expectedBeats = Math.floor(10 * (180 / 60)); // 10 sec * 3 beats/sec
      if (beatCount < expectedBeats * 0.8) {
        warnLog(`Expected ~${expectedBeats} beats at 180 BPM, got ${beatCount}`);
      }
    }
    
    console.log(`   SIM calls: ${simCount}`);
    
    client.close();
    
  } catch (err) {
    errorLog(`Error: ${err.message}`);
    client.close();
    process.exit(1);
  }
}

main();
