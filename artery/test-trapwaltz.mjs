#!/usr/bin/env node
/**
 * Trap Waltz Generator
 * 
 * Combines classical waltz elegance (3/4 time) with trap production
 * Rolling hats, syncopated kicks, but in waltz time signature
 * Uses actual trap drum sounds from notepat
 */

import Artery from './artery.mjs';
import { noteToKey } from '../.vscode/tests/melodies.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const playingLog = (msg) => console.log(`${YELLOW}🎵 ${msg}${RESET}`);

// Drum key mappings (matching notepat.mjs)
const DRUM_KEYS = {
  'kick': { key: 'ArrowDown', code: 'ArrowDown', keyCode: 40 },
  'snare': { key: ' ', code: 'Space', keyCode: 32 },
  'hat': { key: 'ArrowUp', code: 'ArrowUp', keyCode: 38 },
  'hatOpen': { key: 'Alt', code: 'AltRight', keyCode: 18 },
};

// Trap Waltz patterns (12 steps = one bar of 3/4 in 16th note grid)
// Each beat gets 4 16th notes, 3 beats per bar
const TRAPWALTZ_PATTERNS = {
  // Classic trap waltz - rolling hats on waltz time
  'classic': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0], // Beat 1 and pickup to next
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0], // Beat 2 (traditional waltz accent)
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1], // Rolling trap hats
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0], // Open on the "and" of 2
  },
  
  // Dark waltz - heavier, more menacing
  'dark': {
    kick:    [1,0,0,0, 0,0,1,0, 0,0,0,0], // Syncopated kick
    snare:   [0,0,0,0, 1,0,0,0, 1,0,0,0], // Snare on 2 and 3
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1], // Rolling hats
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1], // Opens on every "and"
  },
  
  // Dreamy - sparse, ethereal
  'dreamy': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0], // Just beat 1
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0], // Just beat 2
    hat:     [1,0,1,0, 1,0,1,0, 1,0,1,0], // 8th note hats
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,1], // Open leading to next bar
  },
  
  // Baroque trap - ornamental
  'baroque': {
    kick:    [1,0,0,1, 0,0,0,0, 1,0,0,0], // Dotted rhythm kicks
    snare:   [0,0,0,0, 1,0,0,0, 0,0,1,0], // Syncopated snares
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1], // Full rolling hats
    hatOpen: [0,0,0,0, 0,0,1,0, 0,0,0,1], // Ornamental opens
  },
  
  // Minimal waltz - stripped back
  'minimal': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0], // Snare on 3
    hat:     [0,0,0,0, 1,0,0,0, 0,0,0,0], // Single hat on 2
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  
  // Phonk waltz - Memphis style
  'phonk': {
    kick:    [1,0,0,0, 0,0,1,0, 0,1,0,0], // Bouncy kick
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,1], // Snare with ghost
    hat:     [1,0,1,1, 1,0,1,1, 1,0,1,1], // Triplet feel hats
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,1],
  },
  
  // Viennese - fast, spinning
  'viennese': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0], // Just downbeat
    snare:   [0,0,0,0, 1,0,1,0, 1,0,0,0], // Flams and rolls
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1], // Constant motion
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  
  // Drill waltz - dark UK drill in 3/4
  'drill': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0], // Halftime feel snare
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
};

// Scales for harmonic content
const SCALES = {
  'minor': [0, 2, 3, 5, 7, 8, 10],
  'dorian': [0, 2, 3, 5, 7, 9, 10],
  'phrygian': [0, 1, 3, 5, 7, 8, 10], // Dark, Spanish feel
  'harmonic': [0, 2, 3, 5, 7, 8, 11], // Harmonic minor
  'major': [0, 2, 4, 5, 7, 9, 11],
};

// Chord progressions in waltz style
const PROGRESSIONS = {
  'classical': [0, 3, 4, 4], // i - iv - V - V
  'dark': [0, 5, 6, 4], // i - VI - VII - V
  'romantic': [0, 3, 4, 0], // i - iv - V - i
  'trap': [0, 6, 3, 4], // i - VII - iv - V
};

// Note names
const NOTE_NAMES = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];

// Get note from scale degree (returns note for octave 4 or 5)
function getScaleNote(degree, scale = 'minor', octave = 4) {
  const scaleIntervals = SCALES[scale];
  const noteIndex = ((degree % 7) + 7) % 7;
  const semitones = scaleIntervals[noteIndex];
  
  // Map semitones to diatonic notes
  const semitoneToNote = {
    0: 'c', 2: 'd', 3: 'e', 4: 'e', 5: 'f', 7: 'g', 8: 'a', 9: 'a', 10: 'b', 11: 'b'
  };
  
  const note = semitoneToNote[semitones] || 'c';
  return { note, octave };
}

// Press drum key
async function pressDrumKey(client, drumType, velocity = 100) {
  const drum = DRUM_KEYS[drumType];
  if (!drum) return;
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: '${drum.key}',
          code: '${drum.code}',
          keyCode: ${drum.keyCode},
          which: ${drum.keyCode},
          bubbles: true,
          cancelable: true
        });
        event.velocity = ${velocity};
        window.dispatchEvent(event);
      })()
    `
  });
}

// Release drum key
async function releaseDrumKey(client, drumType) {
  const drum = DRUM_KEYS[drumType];
  if (!drum) return;
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keyup', {
          key: '${drum.key}',
          code: '${drum.code}',
          keyCode: ${drum.keyCode},
          which: ${drum.keyCode},
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

// Press note key
async function pressNoteKey(client, key, velocity = 100) {
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
        event.velocity = ${velocity};
        window.dispatchEvent(event);
      })()
    `
  });
}

// Release note key
async function releaseNoteKey(client, key) {
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

// Generate a complete trap waltz
function generateTrapWaltz(opts = {}) {
  const {
    bars = 8,
    bpm = 140,
    style = 'classic',
    scale = 'minor',
    seed = Date.now(),
    progression = 'classical',
  } = opts;
  
  let randomSeed = seed;
  const seededRandom = () => {
    randomSeed = (randomSeed * 9301 + 49297) % 233280;
    return randomSeed / 233280;
  };
  
  const drumPattern = TRAPWALTZ_PATTERNS[style] || TRAPWALTZ_PATTERNS['classic'];
  const chordProg = PROGRESSIONS[progression] || [0, 3, 4, 0];
  
  const events = [];
  const msPerBeat = 60000 / bpm;
  const msPerBar = msPerBeat * 3; // 3 beats per bar (waltz)
  const msPerStep = msPerBeat / 4; // 16th notes
  
  for (let bar = 0; bar < bars; bar++) {
    const barStart = bar * msPerBar;
    const chordIndex = bar % chordProg.length;
    const chordDegree = chordProg[chordIndex];
    
    // === DRUMS (12 steps per bar in 3/4) ===
    for (let step = 0; step < 12; step++) {
      const stepTime = barStart + (step * msPerStep);
      
      // Kick - uses 'kick' drum type
      if (drumPattern.kick[step]) {
        events.push({
          time: stepTime,
          type: 'kick',
          velocity: step === 0 ? 127 : 100,
          duration: msPerStep * 2,
        });
      }
      
      // Snare - uses 'snare' drum type
      if (drumPattern.snare[step]) {
        const velocity = step === 4 ? 127 : 90; // Beat 2 is strongest
        events.push({
          time: stepTime,
          type: 'snare',
          velocity,
          duration: msPerStep,
        });
      }
      
      // Hi-hat - uses 'hat' drum type
      if (drumPattern.hat[step]) {
        // Trap-style velocity variation
        const accent = (step % 4 === 0) ? 100 : Math.floor(50 + seededRandom() * 40);
        events.push({
          time: stepTime,
          type: 'hat',
          velocity: accent,
          duration: msPerStep * 0.3,
        });
      }
      
      // Open hat - uses 'hatOpen' drum type
      if (drumPattern.hatOpen?.[step]) {
        events.push({
          time: stepTime,
          type: 'hatOpen',
          velocity: 90,
          duration: msPerStep * 1.5,
        });
      }
    }
    
    // === MELODY (waltz-style on beats 1, 2, 3) ===
    // Beat 1 - chord tone
    if (seededRandom() < 0.8) {
      const degrees = [chordDegree + 4, chordDegree + 2, chordDegree];
      const degree = degrees[Math.floor(seededRandom() * degrees.length)];
      const note = getScaleNote(degree, scale, 5); // Octave 5
      events.push({
        time: barStart,
        type: 'melody',
        note: note.note,
        octave: note.octave,
        velocity: 90,
        duration: msPerBeat * 0.8,
      });
    }
    
    // Beat 2 - passing tone
    if (seededRandom() < 0.6) {
      const degree = chordDegree + 1 + Math.floor(seededRandom() * 4);
      const note = getScaleNote(degree, scale, 5);
      events.push({
        time: barStart + msPerBeat,
        type: 'melody',
        note: note.note,
        octave: note.octave,
        velocity: 70,
        duration: msPerBeat * 0.5,
      });
    }
    
    // Beat 3 - resolution
    if (seededRandom() < 0.7) {
      const degree = chordDegree + 2 + Math.floor(seededRandom() * 3);
      const note = getScaleNote(degree, scale, 5);
      events.push({
        time: barStart + msPerBeat * 2,
        type: 'melody',
        note: note.note,
        octave: note.octave,
        velocity: 80,
        duration: msPerBeat * 0.7,
      });
    }
    
    // Bass on beat 1
    const bassNote = getScaleNote(chordDegree, scale, 4);
    events.push({
      time: barStart,
      type: 'melody',
      note: bassNote.note,
      octave: bassNote.octave,
      velocity: 100,
      duration: msPerBeat * 2,
    });
  }
  
  // Sort by time
  events.sort((a, b) => a.time - b.time);
  
  return {
    events,
    totalDuration: bars * msPerBar,
    bpm,
    bars,
    style,
    scale,
    progression,
  };
}

// Play the trap waltz
async function playTrapWaltz(client, waltz) {
  const { events, totalDuration, bpm, bars, style, scale, progression } = waltz;
  
  console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
  console.log(`${YELLOW}🎭 TRAP WALTZ: ${style.toUpperCase()} | ${bpm} BPM | ${scale} | ${bars} bars${RESET}`);
  console.log(`${DIM}   Progression: ${progression}${RESET}`);
  
  // Show pattern
  const pattern = TRAPWALTZ_PATTERNS[style];
  console.log(`\n${DIM}Pattern (12 steps = 1 bar of 3/4):${RESET}`);
  console.log(`  Kick:  ${pattern.kick.map(x => x ? '●' : '○').join('')}`);
  console.log(`  Snare: ${pattern.snare.map(x => x ? '●' : '○').join('')}`);
  console.log(`  Hat:   ${pattern.hat.map(x => x ? '●' : '○').join('')}`);
  if (pattern.hatOpen?.some(x => x)) {
    console.log(`  Open:  ${pattern.hatOpen.map(x => x ? '●' : '○').join('')}`);
  }
  
  playingLog(`Now playing: ${bars} bars of ${style} trap waltz at ${bpm} BPM\n`);
  
  const startTime = Date.now();
  let eventIndex = 0;
  let lastBar = -1;
  const msPerBar = (60000 / bpm) * 3;
  
  // Visual symbols
  const symbols = ['🎭', '💃', '🕺', '🌙', '✨', '🎪', '🦇', '🌹'];
  
  // Track active notes for release
  const activeNotes = new Map();
  
  return new Promise((resolve) => {
    const interval = setInterval(async () => {
      const elapsed = Date.now() - startTime;
      const currentBar = Math.floor(elapsed / msPerBar);
      
      // Bar indicator
      if (currentBar !== lastBar && currentBar < bars) {
        const symbol = symbols[currentBar % symbols.length];
        process.stdout.write(symbol);
        lastBar = currentBar;
      }
      
      // Play events
      while (eventIndex < events.length && events[eventIndex].time <= elapsed) {
        const event = events[eventIndex];
        
        if (event.type === 'kick' || event.type === 'snare' || event.type === 'hat' || event.type === 'hatOpen') {
          // Drum event - use drum keys
          await pressDrumKey(client, event.type, event.velocity);
          setTimeout(async () => {
            await releaseDrumKey(client, event.type);
          }, event.duration);
        } else if (event.type === 'melody') {
          // Melody event - use note keys
          const key = noteToKey(event.note, event.octave);
          if (key) {
            await pressNoteKey(client, key, event.velocity);
            setTimeout(async () => {
              await releaseNoteKey(client, key);
            }, event.duration);
          }
        }
        
        eventIndex++;
      }
      
      // Check if done
      if (elapsed >= totalDuration) {
        clearInterval(interval);
        const actualTime = ((Date.now() - startTime) / 1000).toFixed(1);
        console.log(`\n${GREEN}✓ ${style} complete (${actualTime}s)${RESET}`);
        resolve(actualTime);
      }
    }, 5);
  });
}

// Enable room/reverb
async function pressRoom(client) {
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: '/',
    code: 'Slash',
  });
  await new Promise(r => setTimeout(r, 50));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key: '/',
    code: 'Slash',
  });
}

// Main
async function main() {
  // Parse args
  const args = process.argv.slice(2);
  
  let style = 'classic';
  let bars = 8;
  let bpm = 140;
  let scale = 'minor';
  let seed = Date.now();
  let progression = 'classical';
  let roomMode = false;
  let playAll = false;
  
  // Show help
  console.log(`\n${MAGENTA}🎭 Trap Waltz Generator${RESET}`);
  console.log(`${DIM}Usage: test-trapwaltz [style] [bars=N] [bpm=N] [scale=X] [progression=X] [room] [all]${RESET}`);
  console.log(`  ${DIM}styles: classic, dark, dreamy, baroque, minimal, phonk, viennese, drill${RESET}`);
  console.log(`  ${DIM}scales: minor, dorian, phrygian, harmonic, major${RESET}`);
  console.log(`  ${DIM}progressions: classical, dark, romantic, trap${RESET}`);
  console.log(`  ${DIM}room: enable reverb${RESET}`);
  console.log(`  ${DIM}all: play all styles${RESET}\n`);
  
  for (const arg of args) {
    if (arg.startsWith('bars=')) {
      bars = parseInt(arg.split('=')[1]) || 8;
    } else if (arg.startsWith('bpm=')) {
      bpm = parseInt(arg.split('=')[1]) || 140;
    } else if (arg.startsWith('scale=')) {
      scale = arg.split('=')[1] || 'minor';
    } else if (arg.startsWith('progression=')) {
      progression = arg.split('=')[1] || 'classical';
    } else if (arg.startsWith('seed=')) {
      seed = parseInt(arg.split('=')[1]) || Date.now();
    } else if (arg === 'room') {
      roomMode = true;
    } else if (arg === 'all') {
      playAll = true;
    } else if (TRAPWALTZ_PATTERNS[arg]) {
      style = arg;
    } else if (SCALES[arg]) {
      scale = arg;
    }
  }
  
  testLog('Starting Trap Waltz Generator\n');
  
  try {
    await Artery.openPanelStandalone();
    await new Promise(r => setTimeout(r, 500));
    
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    // Console filter
    const consoleHandler = (type, msg) => {
      if (msg.includes('MIDI') || msg.includes('UDP') || msg.includes('Failed to fetch') ||
          msg.includes('Aesthetic') || msg.includes('WebGPU') || msg.includes('color:') ||
          msg.includes('renderer') || msg.includes('BIOS')) {
        return;
      }
      if (msg.includes('🏠') || msg.includes('ROOM') || msg.includes('REVERB')) {
        console.log(`${CYAN}🔊 [browser] ${msg}${RESET}`);
      }
    };
    await client.enableConsole(consoleHandler);
    
    await client.jump('notepat');
    testLog('Navigated to notepat');
    
    await new Promise(r => setTimeout(r, 2000));
    
    client.close();
    await client.connect();
    testLog('Reconnected after navigation');
    await client.enableConsole();
    
    await new Promise(r => setTimeout(r, 500));
    
    await client.activateAudio();
    testLog('Audio context activated\n');
    
    if (roomMode) {
      await pressRoom(client);
      testLog('🏠 Room mode enabled\n');
      await new Promise(r => setTimeout(r, 100));
    }
    
    if (playAll) {
      console.log(`${MAGENTA}╔═══════════════════════════════════════════════════════╗${RESET}`);
      console.log(`${MAGENTA}║  🎭 PLAYING ALL TRAP WALTZ STYLES                     ║${RESET}`);
      console.log(`${MAGENTA}╚═══════════════════════════════════════════════════════╝${RESET}\n`);
      
      const styles = Object.keys(TRAPWALTZ_PATTERNS);
      let totalTime = 0;
      
      for (const s of styles) {
        const waltz = generateTrapWaltz({
          bars: 4,
          bpm,
          style: s,
          scale,
          seed: seed + styles.indexOf(s),
          progression,
        });
        const elapsed = await playTrapWaltz(client, waltz);
        totalTime += parseFloat(elapsed);
        await new Promise(r => setTimeout(r, 1000));
      }
      
      console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}`);
      console.log(`${GREEN}✅ All styles complete! Total: ${totalTime.toFixed(1)}s${RESET}`);
      console.log(`${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
    } else {
      const waltz = generateTrapWaltz({ bars, bpm, style, scale, seed, progression });
      await playTrapWaltz(client, waltz);
    }
    
    console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
    console.log(`${DIM}💡 Try: test-trapwaltz dark room bpm=120${RESET}`);
    console.log(`${DIM}💡 Try: test-trapwaltz baroque harmonic progression=romantic${RESET}`);
    console.log(`${DIM}💡 Try: test-trapwaltz all room${RESET}\n`);
    
    if (roomMode) {
      await pressRoom(client);
      testLog('🏠 Room mode disabled');
    }
    
    await client.jump('prompt');
    testLog('Returned to prompt');
    
    client.close();
    await Artery.closePanelStandalone();
    
    process.exit(0);
    
  } catch (err) {
    console.error('Error:', err.message);
    process.exit(1);
  }
}

export { main };

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
