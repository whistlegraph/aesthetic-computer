#!/usr/bin/env node
/**
 * Automated Notepat Two-Handed/Chord Player
 * 
 * Plays melodies with chords and harmony (multiple keys at once)
 */

import Artery from '../../artery/artery.mjs';
import { noteToKey } from './melodies.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const playingLog = (msg) => console.log(`${YELLOW}🎵 ${msg}${RESET}`);

// Chord progressions - multiple notes played simultaneously
// Format: {melody: {note, octave}, chord: [{note, octave}, ...], duration}

const waltzes = {
  'simple-waltz': [
    // Bar 1: C major - melody on top, chord underneath
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}, {note: 'g', octave: 4}], duration: 600},
    {melody: {note: 'd', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}], duration: 300},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'g', octave: 4}], duration: 300},
    
    // Bar 2: G major
    {melody: {note: 'd', octave: 5}, chord: [{note: 'g', octave: 4}, {note: 'b', octave: 4}], duration: 600},
    {melody: {note: 'b', octave: 4}, chord: [{note: 'g', octave: 4}], duration: 300},
    {melody: {note: 'g', octave: 4}, chord: [{note: 'd', octave: 4}, {note: 'g', octave: 4}], duration: 300},
    
    // Bar 3: F major
    {melody: {note: 'c', octave: 5}, chord: [{note: 'f', octave: 4}, {note: 'a', octave: 4}], duration: 600},
    {melody: {note: 'a', octave: 4}, chord: [{note: 'f', octave: 4}], duration: 300},
    {melody: {note: 'f', octave: 4}, chord: [{note: 'c', octave: 4}, {note: 'f', octave: 4}], duration: 300},
    
    // Bar 4: C major
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}, {note: 'g', octave: 4}], duration: 600},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}], duration: 300},
    {melody: {note: 'g', octave: 4}, chord: [{note: 'c', octave: 4}, {note: 'g', octave: 4}], duration: 300},
    
    // Bar 5: A minor
    {melody: {note: 'a', octave: 5}, chord: [{note: 'a', octave: 4}, {note: 'c', octave: 5}], duration: 600},
    {melody: {note: 'g', octave: 5}, chord: [{note: 'a', octave: 4}, {note: 'e', octave: 5}], duration: 300},
    {melody: {note: 'e', octave: 5}, chord: [{note: 'e', octave: 4}, {note: 'a', octave: 4}], duration: 300},
    
    // Bar 6: D minor
    {melody: {note: 'f', octave: 5}, chord: [{note: 'd', octave: 4}, {note: 'f', octave: 4}, {note: 'a', octave: 4}], duration: 600},
    {melody: {note: 'd', octave: 5}, chord: [{note: 'd', octave: 4}, {note: 'f', octave: 4}], duration: 300},
    {melody: {note: 'a', octave: 4}, chord: [{note: 'd', octave: 4}, {note: 'a', octave: 4}], duration: 300},
    
    // Bar 7: G major
    {melody: {note: 'b', octave: 4}, chord: [{note: 'g', octave: 4}, {note: 'b', octave: 4}, {note: 'd', octave: 5}], duration: 600},
    {melody: {note: 'd', octave: 5}, chord: [{note: 'g', octave: 4}, {note: 'b', octave: 4}], duration: 300},
    {melody: {note: 'g', octave: 5}, chord: [{note: 'g', octave: 4}, {note: 'd', octave: 5}], duration: 300},
    
    // Bar 8: C major - final resolution
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}, {note: 'g', octave: 4}], duration: 600},
    {melody: {note: 'g', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'g', octave: 4}], duration: 300},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'c', octave: 4}, {note: 'e', octave: 4}, {note: 'g', octave: 4}], duration: 900},
  ],
  
  'arpeggio-waltz': [
    // Bar 1: C major arpeggio with melody
    {melody: {note: 'g', octave: 5}, chord: [{note: 'c', octave: 4}], duration: 400},
    {melody: {note: 'e', octave: 5}, chord: [{note: 'e', octave: 4}], duration: 400},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'g', octave: 4}], duration: 400},
    
    // Bar 2: G major arpeggio
    {melody: {note: 'd', octave: 5}, chord: [{note: 'g', octave: 4}], duration: 400},
    {melody: {note: 'b', octave: 4}, chord: [{note: 'b', octave: 4}], duration: 400},
    {melody: {note: 'g', octave: 4}, chord: [{note: 'd', octave: 4}], duration: 400},
    
    // Bar 3: F major arpeggio
    {melody: {note: 'a', octave: 4}, chord: [{note: 'f', octave: 4}], duration: 400},
    {melody: {note: 'f', octave: 5}, chord: [{note: 'a', octave: 4}], duration: 400},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'c', octave: 4}], duration: 400},
    
    // Bar 4: C major
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 4}], duration: 400},
    {melody: {note: 'g', octave: 4}, chord: [{note: 'e', octave: 4}], duration: 400},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'g', octave: 4}], duration: 400},
    
    // Bar 5: A minor arpeggio
    {melody: {note: 'a', octave: 5}, chord: [{note: 'a', octave: 4}], duration: 400},
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 5}], duration: 400},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'e', octave: 4}], duration: 400},
    
    // Bar 6: D minor arpeggio
    {melody: {note: 'd', octave: 5}, chord: [{note: 'd', octave: 4}], duration: 400},
    {melody: {note: 'f', octave: 5}, chord: [{note: 'f', octave: 4}], duration: 400},
    {melody: {note: 'a', octave: 4}, chord: [{note: 'a', octave: 4}], duration: 400},
    
    // Bar 7: G major arpeggio
    {melody: {note: 'b', octave: 4}, chord: [{note: 'g', octave: 4}], duration: 400},
    {melody: {note: 'g', octave: 5}, chord: [{note: 'b', octave: 4}], duration: 400},
    {melody: {note: 'd', octave: 5}, chord: [{note: 'd', octave: 5}], duration: 400},
    
    // Bar 8: C major final
    {melody: {note: 'e', octave: 5}, chord: [{note: 'c', octave: 4}], duration: 400},
    {melody: {note: 'g', octave: 5}, chord: [{note: 'e', octave: 4}], duration: 400},
    {melody: {note: 'c', octave: 5}, chord: [{note: 'g', octave: 4}], duration: 800},
  ],
  
  'harmony-waltz': [
    // Parallel harmony - melody with third below
    {melody: {note: 'c', octave: 5}, harmony: {note: 'a', octave: 4}, bass: {note: 'c', octave: 4}, duration: 500},
    {melody: {note: 'd', octave: 5}, harmony: {note: 'b', octave: 4}, bass: {note: 'g', octave: 4}, duration: 250},
    {melody: {note: 'e', octave: 5}, harmony: {note: 'c', octave: 5}, bass: {note: 'c', octave: 4}, duration: 250},
    
    {melody: {note: 'g', octave: 5}, harmony: {note: 'e', octave: 5}, bass: {note: 'c', octave: 4}, duration: 500},
    {melody: {note: 'f', octave: 5}, harmony: {note: 'd', octave: 5}, bass: {note: 'g', octave: 4}, duration: 250},
    {melody: {note: 'e', octave: 5}, harmony: {note: 'c', octave: 5}, bass: {note: 'c', octave: 4}, duration: 250},
    
    {melody: {note: 'd', octave: 5}, harmony: {note: 'b', octave: 4}, bass: {note: 'g', octave: 4}, duration: 500},
    {melody: {note: 'c', octave: 5}, harmony: {note: 'a', octave: 4}, bass: {note: 'f', octave: 4}, duration: 250},
    {melody: {note: 'b', octave: 4}, harmony: {note: 'g', octave: 4}, bass: {note: 'g', octave: 4}, duration: 250},
    
    {melody: {note: 'c', octave: 5}, harmony: {note: 'e', octave: 4}, bass: {note: 'c', octave: 4}, duration: 500},
    {melody: {note: 'e', octave: 5}, harmony: {note: 'g', octave: 4}, bass: {note: 'c', octave: 4}, duration: 250},
    {melody: {note: 'g', octave: 5}, harmony: {note: 'e', octave: 5}, bass: {note: 'c', octave: 4}, duration: 250},
    
    // Development section
    {melody: {note: 'a', octave: 5}, harmony: {note: 'f', octave: 5}, bass: {note: 'f', octave: 4}, duration: 500},
    {melody: {note: 'g', octave: 5}, harmony: {note: 'e', octave: 5}, bass: {note: 'c', octave: 4}, duration: 250},
    {melody: {note: 'f', octave: 5}, harmony: {note: 'd', octave: 5}, bass: {note: 'f', octave: 4}, duration: 250},
    
    {melody: {note: 'e', octave: 5}, harmony: {note: 'c', octave: 5}, bass: {note: 'c', octave: 4}, duration: 500},
    {melody: {note: 'd', octave: 5}, harmony: {note: 'b', octave: 4}, bass: {note: 'g', octave: 4}, duration: 250},
    {melody: {note: 'c', octave: 5}, harmony: {note: 'a', octave: 4}, bass: {note: 'c', octave: 4}, duration: 250},
    
    // Final resolution
    {melody: {note: 'g', octave: 5}, harmony: {note: 'e', octave: 5}, bass: {note: 'c', octave: 4}, duration: 600},
    {melody: {note: 'e', octave: 5}, harmony: {note: 'c', octave: 5}, bass: {note: 'c', octave: 4}, duration: 300},
    {melody: {note: 'c', octave: 5}, harmony: {note: 'g', octave: 4}, bass: {note: 'c', octave: 4}, duration: 900},
  ],
};

async function pressKeys(client, keys, duration) {
  // Press all keys down simultaneously
  for (const key of keys) {
    await client.send('Input.dispatchKeyEvent', {
      type: 'keyDown',
      text: key,
      key: key,
      code: `Key${key.toUpperCase()}`,
      windowsVirtualKeyCode: key.charCodeAt(0)
    });
    await new Promise(r => setTimeout(r, 5)); // Tiny stagger for simultaneity
  }
  
  // Hold for duration
  await new Promise(r => setTimeout(r, duration));
  
  // Release all keys
  for (const key of keys) {
    await client.send('Input.dispatchKeyEvent', {
      type: 'keyUp',
      key: key,
      code: `Key${key.toUpperCase()}`,
      windowsVirtualKeyCode: key.charCodeAt(0)
    });
  }
}

async function playWaltz(client, waltzName) {
  const waltz = waltzes[waltzName];
  if (!waltz) {
    console.error(`Unknown waltz: ${waltzName}`);
    return 0;
  }
  
  playingLog(`Now playing: ${waltzName} (${waltz.length} measures)`);
  console.log('');
  
  let notesPlayed = 0;
  
  for (const measure of waltz) {
    const keys = [];
    let displayNotes = [];
    
    // Build the melody note
    if (measure.melody) {
      const melodyKey = noteToKey(measure.melody.note, measure.melody.octave);
      keys.push(melodyKey);
      displayNotes.push(`${YELLOW}${measure.melody.note.toUpperCase()}${measure.melody.octave}${RESET}`);
    }
    
    // Build harmony note if present
    if (measure.harmony) {
      const harmonyKey = noteToKey(measure.harmony.note, measure.harmony.octave);
      keys.push(harmonyKey);
      displayNotes.push(`${CYAN}${measure.harmony.note.toUpperCase()}${measure.harmony.octave}${RESET}`);
    }
    
    // Build bass note if present
    if (measure.bass) {
      const bassKey = noteToKey(measure.bass.note, measure.bass.octave);
      keys.push(bassKey);
      displayNotes.push(`${MAGENTA}${measure.bass.note.toUpperCase()}${measure.bass.octave}${RESET}`);
    }
    
    // Build chord notes if present
    if (measure.chord) {
      for (const chordNote of measure.chord) {
        const chordKey = noteToKey(chordNote.note, chordNote.octave);
        keys.push(chordKey);
        displayNotes.push(`${CYAN}${chordNote.note.toUpperCase()}${chordNote.octave}${RESET}`);
      }
    }
    
    // Display what we're playing
    process.stdout.write(`♪ [${displayNotes.join(' ')}] `);
    
    // Play all keys simultaneously
    await pressKeys(client, keys, measure.duration);
    
    notesPlayed++;
    
    // Line break every 3 measures (waltz time!)
    if (notesPlayed % 3 === 0) {
      console.log('');
    }
    
    // Small gap between chords
    await new Promise(r => setTimeout(r, 30));
  }
  
  if (notesPlayed % 3 !== 0) {
    console.log(''); // Final line break
  }
  
  console.log('');
  return notesPlayed;
}

async function testChords(waltzName = 'simple-waltz') {
  
  try {
    console.log('');
    testLog(`Starting Notepat Two-Handed Player`);
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
    console.log(`${YELLOW}Legend: Yellow=Melody, Cyan=Harmony/Chord, Magenta=Bass${RESET}`);
    console.log('');
    
    const measuresPlayed = await playWaltz(client, waltzName);
    
    console.log(`${CYAN}${'='.repeat(60)}${RESET}`);
    console.log('');
    successLog(`Waltz complete! Played ${measuresPlayed} measures`);
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

// Parse waltz name from command line
const waltzName = process.argv[2] || 'simple-waltz';

console.log(`${CYAN}🎵 Available waltzes: ${Object.keys(waltzes).join(', ')}${RESET}`);

testChords(waltzName);
