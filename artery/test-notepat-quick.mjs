#!/usr/bin/env node
/**
 * 🎹 Notepat Quick Test (Electron Compatible)
 * 
 * A simpler version of test-notepat.mjs that works with both CDP and Electron.
 * Plays a short melody on notepat to verify the connection is working.
 */

import { getArtery } from './artery-auto.mjs';

// Colors
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);

// Note to key mapping (simplified)
const NOTE_KEYS = {
  'c': 'a', 'd': 's', 'e': 'd', 'f': 'f', 'g': 'g', 'a': 'h', 'b': 'j',
  'C': 'q', 'D': 'w', 'E': 'e', 'F': 'r', 'G': 't', 'A': 'y', 'B': 'u',
};

// Simple melody - Twinkle Twinkle first phrase
const MELODY = [
  { note: 'c', duration: 400 },
  { note: 'c', duration: 400 },
  { note: 'g', duration: 400 },
  { note: 'g', duration: 400 },
  { note: 'a', duration: 400 },
  { note: 'a', duration: 400 },
  { note: 'g', duration: 800 },
];

async function pressKey(client, key) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.toUpperCase().charCodeAt(0);
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: '${key}',
          code: '${code}',
          keyCode: ${keyCode},
          which: ${keyCode},
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

async function releaseKey(client, key) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.toUpperCase().charCodeAt(0);
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keyup', {
          key: '${key}',
          code: '${code}',
          keyCode: ${keyCode},
          which: ${keyCode},
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

async function playNote(client, note, duration) {
  const key = NOTE_KEYS[note];
  if (!key) return;
  
  await pressKey(client, key);
  await new Promise(r => setTimeout(r, Math.min(duration, 200)));
  await releaseKey(client, key);
  await new Promise(r => setTimeout(r, duration - 200));
}

async function main() {
  testLog('Notepat Quick Test (Electron Compatible)\n');
  
  try {
    // Get the appropriate Artery class
    const Artery = await getArtery();
    
    // Open panel (no-op in Electron)
    await Artery.openPanelStandalone();
    await new Promise(r => setTimeout(r, 500));
    
    // Connect
    const client = new Artery();
    await client.connect();
    testLog('Connected');
    
    // Navigate to notepat
    await client.jump('notepat');
    testLog('Navigated to notepat');
    
    // Wait for piece to load
    await new Promise(r => setTimeout(r, 2000));
    
    // Activate audio
    await client.activateAudio();
    testLog('Audio activated');
    
    await new Promise(r => setTimeout(r, 500));
    
    // Play melody
    console.log(`\n${CYAN}🎵 Playing Twinkle Twinkle...${RESET}\n`);
    
    for (const { note, duration } of MELODY) {
      process.stdout.write(`${YELLOW}♪${RESET}`);
      await playNote(client, note, duration);
    }
    
    console.log('\n');
    successLog('Melody complete!');
    
    // Return to prompt
    await new Promise(r => setTimeout(r, 1000));
    await client.jump('prompt');
    testLog('Returned to prompt');
    
    client.close();
    await Artery.closePanelStandalone();
    
    successLog('Test passed!\n');
    process.exit(0);
    
  } catch (err) {
    console.error(`\n${YELLOW}Error:${RESET}`, err.message);
    process.exit(1);
  }
}

main();
