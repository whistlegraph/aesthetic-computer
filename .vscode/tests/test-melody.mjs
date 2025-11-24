#!/usr/bin/env node
/**
 * Automated Notepat Melody Player Test
 * 
 * Plays famous melodies through notepat
 */

import Artery from '../artery.mjs';
import { melodies, noteToKey } from './melodies.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);

async function testMelody(melodyName = 'twinkle') {
  
  try {
    console.log('');
    testLog(`Starting Notepat Melody Player: ${melodyName}`);
    console.log('');
    
    const melody = melodies[melodyName];
    if (!melody) {
      console.error(`Unknown melody: ${melodyName}`);
      console.log(`Available melodies: ${Object.keys(melodies).join(', ')}`);
      process.exit(1);
    }
    
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
    
    console.log('');
    testLog(`Playing ${melody.length} notes...`);
    console.log('');
    
    // Play melody note by note with display
    let notesPlayed = 0;
    for (const {note, octave, duration} of melody) {
      const key = noteToKey(note, octave);
      const noteDisplay = `${note.toUpperCase()}${octave}`;
      
      // Show the note being played
      process.stdout.write(`${CYAN}♪ ${noteDisplay.padEnd(4)} ${RESET}`);
      
      // Dispatch keydown
      await client.send('Input.dispatchKeyEvent', {
        type: 'keyDown',
        text: key,
        key: key,
        code: `Key${key.toUpperCase()}`,
        windowsVirtualKeyCode: key.charCodeAt(0)
      });
      
      await new Promise(r => setTimeout(r, duration));
      
      // Dispatch keyup
      await client.send('Input.dispatchKeyEvent', {
        type: 'keyUp',
        key: key,
        code: `Key${key.toUpperCase()}`,
        windowsVirtualKeyCode: key.charCodeAt(0)
      });
      
      notesPlayed++;
      
      // Line break every 8 notes
      if (notesPlayed % 8 === 0) {
        console.log('');
      }
      
      // Small gap between notes
      await new Promise(r => setTimeout(r, 50));
    }
    
    console.log('');
    successLog(`Melody complete! Played ${notesPlayed} notes`);
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
    console.error(`💔 Test failed: ${error.message}`);
    process.exit(1);
  }
}

// Parse melody name from command line
const melodyName = process.argv[2] || 'twinkle';

console.log(`${CYAN}🎵 Available melodies: ${Object.keys(melodies).join(', ')}${RESET}`);

testMelody(melodyName);
