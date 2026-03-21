#!/usr/bin/env node
/**
 * Automated Notepat Playlist Player
 * 
 * Plays a playlist of folk and traditional melodies
 */

import Artery from '../../artery/artery.mjs';
import { melodies, noteToKey } from './melodies.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const playingLog = (msg) => console.log(`${YELLOW}🎵 ${msg}${RESET}`);

// Default playlist: all folk/traditional songs
const defaultPlaylist = [
  'twinkle',
  'mary',
  'old-macdonald',
  'yankee-doodle',
  'frere-jacques',
  'london-bridge',
  'row-row-row',
  'skip-to-my-lou',
  'camptown-races',
  'oh-susanna',
  'amazing-grace',
  'auld-lang-syne',
  'shenandoah',
  'home-on-the-range',
  'red-river-valley',
  'scarborough-fair',
  'greensleeves',
  'when-the-saints',
  'danny-boy',
  'ode-to-joy',
  'jingle-bells',
  'happy-birthday',
];

async function playMelody(client, melodyName) {
  const melody = melodies[melodyName];
  if (!melody) {
    console.error(`Unknown melody: ${melodyName}`);
    return 0;
  }
  
  playingLog(`Now playing: ${melodyName} (${melody.length} notes)`);
  
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
  
  if (notesPlayed % 8 !== 0) {
    console.log(''); // Final line break
  }
  
  console.log('');
  return notesPlayed;
}

async function playPlaylist(playlist = defaultPlaylist) {
  
  try {
    console.log('');
    testLog(`Starting Notepat Playlist Player`);
    playingLog(`Playlist: ${playlist.length} melodies`);
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
    
    console.log('');
    console.log(`${CYAN}${'='.repeat(60)}${RESET}`);
    console.log('');
    
    let totalNotes = 0;
    let songsPlayed = 0;
    
    // Play each melody in the playlist
    for (const melodyName of playlist) {
      const notesPlayed = await playMelody(client, melodyName);
      totalNotes += notesPlayed;
      songsPlayed++;
      
      // Pause between songs
      await new Promise(r => setTimeout(r, 1000));
    }
    
    console.log(`${CYAN}${'='.repeat(60)}${RESET}`);
    console.log('');
    successLog(`Playlist complete! Played ${songsPlayed} songs, ${totalNotes} total notes`);
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
    console.error(`💔 Playlist failed: ${error.message}`);
    process.exit(1);
  }
}

// Parse custom playlist from command line or use default
const args = process.argv.slice(2);
let playlist;

if (args.length > 0) {
  // Custom playlist from command line
  playlist = args;
  console.log(`${CYAN}🎵 Custom playlist: ${playlist.join(', ')}${RESET}`);
} else {
  // Use default playlist
  playlist = defaultPlaylist;
  console.log(`${CYAN}🎵 Playing all ${playlist.length} melodies${RESET}`);
}

playPlaylist(playlist);
