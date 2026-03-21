#!/usr/bin/env node
/**
 * Hip-Hop Beat Test
 * 
 * Beat-centric design - drums drive the track with sparse melodic elements
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

// Hip-hop beat patterns (16th note grid, 1 = hit, 0 = rest)
// Each pattern is 16 steps (one bar of 4/4)
const BEAT_PATTERNS = {
  // Classic boom bap
  'boombap': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,0,1, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
  },
  
  // Trap style - rolling hats
  'trap': {
    kick:  [1,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
  
  // Laid back / lo-fi
  'lofi': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,1],
    hat:   [0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0],
  },
  
  // J Dilla style (slightly off-grid feel)
  'dilla': {
    kick:  [1,0,0,0, 0,0,0,1, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,0,1, 0,1,0,0, 1,0,0,1, 0,1,0,0],
  },
  
  // 808 heavy
  '808': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,1],
  },
  
  // Half-time / drill
  'halftime': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,1,0],
    snare: [0,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0],
    hat:   [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
  },
  
  // New York underground
  'nyc': {
    kick:  [1,0,0,0, 0,0,1,0, 0,0,0,0, 1,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,1, 0,0,0,0],
  },
  
  // Dirty South bounce
  'south': {
    kick:  [1,0,0,1, 0,0,1,0, 1,0,0,1, 0,0,1,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
  },
  
  // West coast G-funk
  'gfunk': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0],
    snare: [0,0,0,0, 1,0,0,1, 0,0,0,0, 1,0,0,0],
    hat:   [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
  },
  
  // Phonk / Memphis
  'phonk': {
    kick:  [1,0,0,0, 0,0,1,0, 0,1,0,0, 0,0,1,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,1],
    hat:   [1,0,1,1, 1,0,1,1, 1,0,1,1, 1,0,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
  
  // UK garage / 2-step
  'garage': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 0,1,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
  },
  
  // Minimal / stripped
  'minimal': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  
  // Afrobeats influence
  'afro': {
    kick:  [1,0,0,0, 0,0,1,0, 0,0,0,0, 1,0,0,0],
    snare: [0,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
  
  // Reggaeton dembow
  'dembow': {
    kick:  [1,0,0,1, 0,0,1,0, 1,0,0,1, 0,0,1,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
  
  // Breakbeat
  'break': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,1, 0,0,0,0, 1,0,1,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
  },
};

// Bass patterns (scale degrees, 0 = rest)
const BASS_PATTERNS = {
  'simple': [
    { degree: 0, duration: 4 },
    { degree: 0, duration: 4 },
    { degree: -2, duration: 4 },
    { degree: 0, duration: 4 },
  ],
  'bounce': [
    { degree: 0, duration: 2 },
    { degree: 0, duration: 1 },
    { degree: 0, duration: 1 },
    { degree: -2, duration: 2 },
    { degree: -3, duration: 2 },
  ],
  'minimal': [
    { degree: 0, duration: 8 },
    { degree: -2, duration: 8 },
  ],
  'rhythmic': [
    { degree: 0, duration: 3 },
    { degree: 0, duration: 1 },
    { degree: -2, duration: 2 },
    { degree: -3, duration: 1 },
    { degree: -2, duration: 1 },
  ],
};

// Melodic hooks (sparse, memorable)
const HOOK_PATTERNS = {
  'sparse': [
    // Just a few notes per bar
    { step: 0, degree: 4, duration: 4 },
    { step: 8, degree: 2, duration: 4 },
  ],
  'call': [
    { step: 0, degree: 4, duration: 2 },
    { step: 2, degree: 5, duration: 2 },
    { step: 4, degree: 4, duration: 4 },
  ],
  'response': [
    { step: 8, degree: 2, duration: 2 },
    { step: 10, degree: 0, duration: 2 },
    { step: 12, degree: 2, duration: 4 },
  ],
  'minimal': [
    { step: 0, degree: 4, duration: 8 },
  ],
  'trippy': [
    { step: 0, degree: 4, duration: 2 },
    { step: 3, degree: 5, duration: 2 },
    { step: 6, degree: 7, duration: 2 },
    { step: 12, degree: 4, duration: 4 },
  ],
  'arp': [
    { step: 0, degree: 0, duration: 2 },
    { step: 2, degree: 2, duration: 2 },
    { step: 4, degree: 4, duration: 2 },
    { step: 6, degree: 5, duration: 2 },
    { step: 8, degree: 4, duration: 2 },
    { step: 10, degree: 2, duration: 2 },
    { step: 12, degree: 0, duration: 4 },
  ],
  'stab': [
    { step: 0, degree: 4, duration: 1 },
    { step: 4, degree: 4, duration: 1 },
    { step: 8, degree: 5, duration: 1 },
    { step: 12, degree: 4, duration: 1 },
  ],
  'run': [
    { step: 0, degree: 0, duration: 1 },
    { step: 1, degree: 1, duration: 1 },
    { step: 2, degree: 2, duration: 1 },
    { step: 3, degree: 3, duration: 1 },
    { step: 4, degree: 4, duration: 4 },
    { step: 8, degree: 4, duration: 1 },
    { step: 9, degree: 3, duration: 1 },
    { step: 10, degree: 2, duration: 1 },
    { step: 11, degree: 1, duration: 1 },
    { step: 12, degree: 0, duration: 4 },
  ],
  'syncopate': [
    { step: 1, degree: 4, duration: 3 },
    { step: 5, degree: 5, duration: 3 },
    { step: 9, degree: 2, duration: 3 },
    { step: 13, degree: 0, duration: 3 },
  ],
  'bounce': [
    { step: 0, degree: 0, duration: 2 },
    { step: 3, degree: 0, duration: 1 },
    { step: 4, degree: 2, duration: 2 },
    { step: 7, degree: 2, duration: 1 },
    { step: 8, degree: 4, duration: 2 },
    { step: 11, degree: 4, duration: 1 },
    { step: 12, degree: 5, duration: 4 },
  ],
};

const NOTE_NAMES = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];
const SCALES = {
  'minor': [0, 2, 3, 5, 7, 8, 10],
  'dorian': [0, 2, 3, 5, 7, 9, 10],
  'phrygian': [0, 1, 3, 5, 7, 8, 10],
};

function getScaleNote(degree, scale = 'minor', octave = 3) {
  const scaleIntervals = SCALES[scale];
  const noteIndex = ((degree % 7) + 7) % 7;
  const octaveShift = Math.floor(degree / 7);
  const semitones = scaleIntervals[noteIndex];
  const chromaticToNote = ['c', null, 'd', null, 'e', 'f', null, 'g', null, 'a', null, 'b'];
  const note = chromaticToNote[semitones % 12];
  return note ? { note, octave: octave + octaveShift } : null;
}

// Get swing timing offsets
function getSwingTimings(swing, stepDuration) {
  // swing 50 = straight, 66 = triplet, 75+ = heavy
  const swingRatio = swing / 100;
  const swingOffset = Math.floor(stepDuration * (swingRatio - 0.5) * 0.5);
  return { swingOffset };
}

// Main beat generator
async function generateHipHop(bars = 4, bpm = 90, patternName = 'boombap', scale = 'minor', seed = Date.now(), swing = 50, melodicDensity = 'sparse') {
  // Seeded random
  let randomSeed = seed;
  const seededRandom = () => {
    randomSeed = (randomSeed * 9301 + 49297) % 233280;
    return randomSeed / 233280;
  };
  
  const pattern = BEAT_PATTERNS[patternName] || BEAT_PATTERNS.boombap;
  const bassPattern = Object.values(BASS_PATTERNS)[Math.floor(seededRandom() * Object.keys(BASS_PATTERNS).length)];
  
  // Get all hook patterns for variety
  const hookPatternKeys = Object.keys(HOOK_PATTERNS);
  const primaryHookPattern = HOOK_PATTERNS[melodicDensity] || HOOK_PATTERNS.sparse;
  
  // Create a melodic sequence that varies by section (verse/chorus/bridge)
  const getMelodyPatternForBar = (bar) => {
    const section = Math.floor(bar / 4) % 4; // 4-bar sections
    const barInSection = bar % 4;
    
    if (section === 0) {
      // Verse - sparse, minimal
      return barInSection === 0 ? HOOK_PATTERNS.sparse : (barInSection === 2 ? HOOK_PATTERNS.response : null);
    } else if (section === 1) {
      // Pre-chorus - building
      return [HOOK_PATTERNS.call, HOOK_PATTERNS.syncopate, HOOK_PATTERNS.stab, HOOK_PATTERNS.bounce][barInSection % 4];
    } else if (section === 2) {
      // Chorus - full energy
      return [HOOK_PATTERNS.arp, HOOK_PATTERNS.run, HOOK_PATTERNS.trippy, HOOK_PATTERNS.bounce][barInSection % 4];
    } else {
      // Bridge/outro - back to sparse
      return barInSection < 2 ? HOOK_PATTERNS.minimal : HOOK_PATTERNS.sparse;
    }
  };
  
  // Timing: at 90 BPM, quarter note = 667ms, 16th note = 167ms
  const quarterNote = 60000 / bpm;
  const sixteenthNote = quarterNote / 4;
  const barDuration = quarterNote * 4;
  
  const { swingOffset } = getSwingTimings(swing, sixteenthNote);
  
  const events = [];
  let time = 0;
  
  for (let bar = 0; bar < bars; bar++) {
    // Generate drum hits
    for (let step = 0; step < 16; step++) {
      // Apply swing to off-beats (odd 8th notes)
      let stepTime = time + (step * sixteenthNote);
      if (step % 2 === 1) {
        stepTime += swingOffset;
      }
      
      // Kick
      if (pattern.kick[step]) {
        events.push({
          time: stepTime,
          type: 'kick',
          velocity: step === 0 ? 127 : 100 + Math.floor(seededRandom() * 27),
        });
      }
      
      // Snare
      if (pattern.snare[step]) {
        events.push({
          time: stepTime,
          type: 'snare',
          velocity: 100 + Math.floor(seededRandom() * 27),
        });
      }
      
      // Hi-hat
      if (pattern.hat[step]) {
        events.push({
          time: stepTime,
          type: 'hat',
          velocity: 60 + Math.floor(seededRandom() * 40),
        });
      }
      
      // Open hi-hat
      if (pattern.hatOpen && pattern.hatOpen[step]) {
        events.push({
          time: stepTime,
          type: 'ride', // Using ride for open hat
          velocity: 80 + Math.floor(seededRandom() * 20),
        });
      }
    }
    
    // Generate bass notes
    let bassTime = time;
    for (const bassNote of bassPattern) {
      const note = getScaleNote(bassNote.degree, scale, 2);
      if (note) {
        events.push({
          time: bassTime,
          type: 'bass',
          note: `${note.note}${note.octave}`,
          duration: bassNote.duration * sixteenthNote,
        });
      }
      bassTime += bassNote.duration * sixteenthNote;
      if (bassTime >= time + barDuration) break;
    }
    
    // Generate melodic hook - varies by bar for musical development
    const hookPattern = getMelodyPatternForBar(bar);
    if (hookPattern) {
      // Add some octave variation based on position
      const octaveShift = (bar % 8 < 4) ? 0 : (seededRandom() > 0.5 ? 1 : 0);
      
      for (const hookNote of hookPattern) {
        const note = getScaleNote(hookNote.degree, scale, 4 + octaveShift);
        if (note) {
          events.push({
            time: time + (hookNote.step * sixteenthNote),
            type: 'melody',
            note: `${note.note}${note.octave}`,
            duration: hookNote.duration * sixteenthNote,
          });
        }
      }
    }
    
    time += barDuration;
  }
  
  // Sort by time
  events.sort((a, b) => a.time - b.time);
  
  return {
    events,
    duration: bars * barDuration,
    bars,
    bpm,
    pattern: patternName,
    swing,
  };
}

// Drum key mappings (matching notepat.mjs)
const DRUM_KEYS = {
  'kick': { key: 'ArrowDown', code: 'ArrowDown', keyCode: 40 },
  'snare': { key: ' ', code: 'Space', keyCode: 32 },
  'hat': { key: 'ArrowUp', code: 'ArrowUp', keyCode: 38 },
  'ride': { key: 'Alt', code: 'AltRight', keyCode: 18 },
  'crash': { key: 'Alt', code: 'AltLeft', keyCode: 18 },
  'tomLow': { key: 'ArrowLeft', code: 'ArrowLeft', keyCode: 37 },
  'tomHigh': { key: 'ArrowRight', code: 'ArrowRight', keyCode: 39 },
};

// Press a drum key with velocity
async function pressDrumKey(client, drumType, velocity = 127) {
  const drum = DRUM_KEYS[drumType];
  if (!drum) return;
  
  // Dispatch a custom KeyboardEvent with velocity property
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
        
        // Auto-release after short delay for drums
        setTimeout(() => {
          const upEvent = new KeyboardEvent('keyup', {
            key: '${drum.key}',
            code: '${drum.code}',
            keyCode: ${drum.keyCode},
            which: ${drum.keyCode},
            bubbles: true,
            cancelable: true
          });
          window.dispatchEvent(upEvent);
        }, 20);
      })()
    `
  });
}

// Press a note key with velocity
async function pressNoteKey(client, key, velocity = 127) {
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

// Release a note key
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

// Play a note with duration
async function playNote(client, noteStr, duration = 200, velocity = 100) {
  // noteStr format: "c4" or "g3"
  const match = noteStr.match(/([a-g])(\d)/);
  if (!match) return;
  
  const note = match[1];
  const octave = parseInt(match[2]);
  const key = noteToKey(note, octave);
  if (!key) return;
  
  await pressNoteKey(client, key, velocity);
  await new Promise(r => setTimeout(r, Math.min(duration, 200)));
  await releaseNoteKey(client, key);
}

// Press Tab to cycle wave type
async function pressTab(client) {
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: 'Tab',
          code: 'Tab',
          keyCode: 9,
          which: 9,
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
        setTimeout(() => {
          const upEvent = new KeyboardEvent('keyup', {
            key: 'Tab',
            code: 'Tab',
            keyCode: 9,
            which: 9,
            bubbles: true,
            cancelable: true
          });
          window.dispatchEvent(upEvent);
        }, 20);
      })()
    `
  });
}

// Press Shift for quick mode (left) or slide mode (right)
async function pressShift(client, side = 'left') {
  const code = side === 'left' ? 'ShiftLeft' : 'ShiftRight';
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: 'Shift',
          code: '${code}',
          keyCode: 16,
          which: 16,
          shiftKey: true,
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

async function releaseShift(client, side = 'left') {
  const code = side === 'left' ? 'ShiftLeft' : 'ShiftRight';
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keyup', {
          key: 'Shift',
          code: '${code}',
          keyCode: 16,
          which: 16,
          shiftKey: false,
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

// Press number key for octave change
async function pressNumber(client, num) {
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: '${num}',
          code: 'Digit${num}',
          keyCode: ${48 + num},
          which: ${48 + num},
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
        setTimeout(() => {
          const upEvent = new KeyboardEvent('keyup', {
            key: '${num}',
            code: 'Digit${num}',
            keyCode: ${48 + num},
            which: ${48 + num},
            bubbles: true,
            cancelable: true
          });
          window.dispatchEvent(upEvent);
        }, 20);
      })()
    `
  });
}

// Press < (comma) or > (period) for relative octave shifts
async function pressOctaveShift(client, direction) {
  const key = direction === 'up' ? '.' : ',';
  const code = direction === 'up' ? 'Period' : 'Comma';
  const keyCode = direction === 'up' ? 190 : 188;
  
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
        setTimeout(() => {
          const upEvent = new KeyboardEvent('keyup', {
            key: '${key}',
            code: '${code}',
            keyCode: ${keyCode},
            which: ${keyCode},
            bubbles: true,
            cancelable: true
          });
          window.dispatchEvent(upEvent);
        }, 20);
      })()
    `
  });
}

// 🏠 Room/reverb mode toggle - simulates '/' key press that notepat uses
async function pressRoom(client) {
  // Use Input.dispatchKeyEvent which is more reliable for CDP
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: '/',
    code: 'Slash',
    windowsVirtualKeyCode: 191,
    nativeVirtualKeyCode: 191,
  });
  await new Promise(r => setTimeout(r, 50));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp', 
    key: '/',
    code: 'Slash',
    windowsVirtualKeyCode: 191,
    nativeVirtualKeyCode: 191,
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// WORD MELODY SYSTEM - Using QWERTY keyboard layout as melodic seeds
// ═══════════════════════════════════════════════════════════════════════════
// notepat maps keys to notes:
// Lower octave: c=c, v=c#, d=d, s=d#, e=e, f=f, w=f#, g=g, r=g#, a=a, q=a#, b=b
// Upper octave: h=+c, t=+c#, i=+d, y=+d#, j=+e, k=+f, u=+f#, l=+g, o=+g#, m=+a, p=+a#, n=+b

// Hip-hop / emotional word seeds - these become melodies!
const WORD_SEEDS = {
  // Short punchy words for hooks
  hook: ['bag', 'dab', 'ace', 'fade', 'cage', 'dead', 'beef', 'bead', 'face'],
  
  // Longer melodic words
  melodic: ['facade', 'decade', 'backed', 'decaf', 'beefed', 'deface', 'efface'],
  
  // Words using upper octave (h,i,j,k,l,m,n,o,p,t,u,y)
  high: ['him', 'jump', 'junk', 'myth', 'hymn', 'hint', 'link', 'milk', 'pink', 'tulip'],
  
  // Mix of both ranges
  full: ['hijack', 'knight', 'flight', 'typhoon', 'kingdom'],
  
  // Trap vibes
  trap: ['drip', 'flip', 'clip', 'grip', 'chip', 'trip', 'whip'],
  
  // Emotional
  mood: ['ache', 'aged', 'edge', 'fade', 'jade', 'cage', 'rage'],
  
  // Run/arpeggio patterns (ascending/descending letters)
  run: ['abcdefg', 'gfedcba', 'cdefgab', 'cegbdfac'],
  
  // Ornaments / trills (repeated notes)
  trill: ['aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'abab', 'cdcd', 'efef'],
};

// Play a word as a melody - each letter becomes a note
async function playWord(client, word, noteDuration = 100, velocity = 100, delayBetween = 50) {
  const letters = word.toLowerCase().split('');
  
  for (const letter of letters) {
    // Skip non-letter characters
    if (!/[a-z]/.test(letter)) continue;
    
    await pressNoteKey(client, letter, velocity);
    await new Promise(r => setTimeout(r, noteDuration));
    await releaseNoteKey(client, letter);
    await new Promise(r => setTimeout(r, delayBetween));
  }
}

// Play a word with rhythmic variation
async function playWordRhythmic(client, word, baseVelocity = 100, swing = false) {
  const letters = word.toLowerCase().split('');
  
  for (let i = 0; i < letters.length; i++) {
    const letter = letters[i];
    if (!/[a-z]/.test(letter)) continue;
    
    // Accent first and every other note
    const velocity = (i === 0 || i % 2 === 0) ? baseVelocity : Math.floor(baseVelocity * 0.7);
    
    // Swing timing
    const duration = swing && i % 2 === 1 ? 80 : 120;
    const delay = swing && i % 2 === 1 ? 30 : 60;
    
    await pressNoteKey(client, letter, velocity);
    await new Promise(r => setTimeout(r, duration));
    await releaseNoteKey(client, letter);
    await new Promise(r => setTimeout(r, delay));
  }
}

// Play word as a chord (all notes at once)
async function playWordChord(client, word, duration = 300, velocity = 90) {
  const letters = [...new Set(word.toLowerCase().split('').filter(l => /[a-z]/.test(l)))];
  
  // Press all notes
  for (const letter of letters) {
    await pressNoteKey(client, letter, velocity);
  }
  
  await new Promise(r => setTimeout(r, duration));
  
  // Release all notes
  for (const letter of letters) {
    await releaseNoteKey(client, letter);
  }
}

// Get a random word from a category
function getRandomWord(category = 'hook') {
  const words = WORD_SEEDS[category] || WORD_SEEDS.hook;
  return words[Math.floor(Math.random() * words.length)];
}

// Velocity curves for different vibes - returns velocity based on step position
// step = current 16th note position (0-15 within bar)
// globalStep = total steps from start
// totalSteps = total steps in track
const VELOCITY_CURVES = {
  'flat': () => ({ kick: 100, snare: 100, hat: 80, ride: 90, melody: 90 }),
  
  'accent': (step) => ({
    kick: step === 0 || step === 8 ? 127 : 95,
    snare: 110,
    hat: step % 4 === 0 ? 90 : 60,
    ride: 85,
    melody: step === 0 ? 110 : 85,
  }),
  
  'crescendo': (step, globalStep, totalSteps) => {
    const progress = globalStep / totalSteps;
    return {
      kick: Math.floor(60 + progress * 67),
      snare: Math.floor(50 + progress * 77),
      hat: Math.floor(30 + progress * 60),
      ride: Math.floor(40 + progress * 60),
      melody: Math.floor(50 + progress * 77),
    };
  },
  
  'decrescendo': (step, globalStep, totalSteps) => {
    const progress = 1 - (globalStep / totalSteps);
    return {
      kick: Math.floor(60 + progress * 67),
      snare: Math.floor(50 + progress * 77),
      hat: Math.floor(30 + progress * 60),
      ride: Math.floor(40 + progress * 60),
      melody: Math.floor(50 + progress * 77),
    };
  },
  
  'bounce': (step) => ({
    kick: step === 0 ? 127 : step === 8 ? 115 : 100,
    snare: 110,
    hat: step % 2 === 0 ? 75 : 55,
    ride: 80,
    melody: step % 4 === 0 ? 100 : 75,
  }),
  
  'humanize': (step) => ({
    kick: 90 + Math.floor(Math.random() * 37),
    snare: 85 + Math.floor(Math.random() * 42),
    hat: 50 + Math.floor(Math.random() * 45),
    ride: 60 + Math.floor(Math.random() * 40),
    melody: 70 + Math.floor(Math.random() * 50),
  }),
  
  'ghost': (step) => ({
    kick: step === 0 ? 127 : step === 8 ? 110 : 85,
    snare: step === 4 || step === 12 ? 115 : 70 + Math.floor(Math.random() * 25),
    hat: step % 4 === 0 ? 70 : 35 + Math.floor(Math.random() * 30),
    ride: 75,
    melody: 60 + Math.floor(Math.random() * 40),
  }),
  
  // pressure - builds in 4-bar waves
  'pressure': (step, globalStep, totalSteps) => {
    const phrasePosition = (globalStep % 64) / 64;
    const intensity = Math.sin(phrasePosition * Math.PI);
    return {
      kick: Math.floor(70 + intensity * 57),
      snare: Math.floor(60 + intensity * 67),
      hat: Math.floor(30 + intensity * 60),
      ride: Math.floor(40 + intensity * 50),
      melody: Math.floor(50 + intensity * 70),
    };
  },
  
  // swung - accents follow swing pattern
  'swung': (step) => ({
    kick: step === 0 || step === 10 ? 127 : 90,
    snare: 105,
    hat: step % 2 === 1 ? 85 : 55,
    ride: step % 2 === 1 ? 80 : 60,
    melody: step % 2 === 1 ? 95 : 70,
  }),
  
  // breathe - slow 8-bar inhale/exhale
  'breathe': (step, globalStep, totalSteps) => {
    const breathCycle = (globalStep % 128) / 128;
    const breath = Math.sin(breathCycle * Math.PI * 2) * 0.5 + 0.5;
    return {
      kick: Math.floor(50 + breath * 77),
      snare: Math.floor(40 + breath * 87),
      hat: Math.floor(20 + breath * 70),
      ride: Math.floor(30 + breath * 60),
      melody: Math.floor(40 + breath * 80),
    };
  },
  
  // drop - quiet verse, loud chorus every 8 bars
  'drop': (step, globalStep, totalSteps) => {
    const inChorus = Math.floor(globalStep / 128) % 2 === 1;
    const mult = inChorus ? 1.0 : 0.5;
    return {
      kick: Math.floor(127 * mult),
      snare: Math.floor(120 * mult),
      hat: Math.floor(90 * mult),
      ride: Math.floor(85 * mult),
      melody: Math.floor(110 * mult),
    };
  },
  
  // ramp - continuous build throughout track
  'ramp': (step, globalStep, totalSteps) => {
    const progress = globalStep / totalSteps;
    const curved = Math.pow(progress, 0.7); // Faster initial rise
    return {
      kick: Math.floor(40 + curved * 87),
      snare: Math.floor(30 + curved * 97),
      hat: Math.floor(20 + curved * 70),
      ride: Math.floor(25 + curved * 65),
      melody: Math.floor(35 + curved * 90),
    };
  },
};

// Play a single pattern with enhancements
async function playPattern(client, patternName, bars, bpm, scale, seed, swing, density, velocityCurve = 'humanize', waveChanges = false, wordMode = false, wordSeed = null) {
  const track = await generateHipHop(bars, bpm, patternName, scale, seed, swing, density);
  const beatPattern = BEAT_PATTERNS[patternName];
  const velocityFn = VELOCITY_CURVES[velocityCurve] || VELOCITY_CURVES.humanize;
  
  console.log('═'.repeat(60) + '\n');
  console.log(`${RED}🔊 BEAT: ${patternName.toUpperCase()}${RESET} | ${bpm} BPM | ${scale} | swing=${swing}%`);
  console.log(`${DIM}Velocity: ${velocityCurve} | Wave changes: ${waveChanges ? 'ON' : 'OFF'} | Words: ${wordMode ? (wordSeed || 'random') : 'OFF'}${RESET}\n`);
  
  console.log(`${YELLOW}Pattern (16th notes):${RESET}`);
  console.log(`  Kick:  ${beatPattern.kick.map(x => x ? '●' : '○').join('')}`);
  console.log(`  Snare: ${beatPattern.snare.map(x => x ? '●' : '○').join('')}`);
  console.log(`  Hat:   ${beatPattern.hat.map(x => x ? '●' : '○').join('')}`);
  if (beatPattern.hatOpen) {
    console.log(`  Open:  ${beatPattern.hatOpen.map(x => x ? '●' : '○').join('')}`);
  }
  console.log('');
  
  playingLog(`Now playing: ${bars} bars of ${patternName} at ${bpm} BPM\n`);
  
  const startTime = Date.now();
  let lastEventIndex = 0;
  let globalStep = 0;
  const totalSteps = bars * 16;
  let lastWaveChange = 0;
  const sixteenthNote = (60000 / bpm) / 4;
  const waveChangeInterval = sixteenthNote * 8; // Change wave every half bar (8 sixteenths)
  
  // Mode state
  let quickModeActive = false;
  let slideModeActive = false;
  let currentOctave = 5; // Default octave
  let lastOctaveChange = 0;
  let lastModeChange = 0;
  const octaveChangeInterval = sixteenthNote * 32; // Change octave every 2 bars
  const modeToggleInterval = sixteenthNote * 16; // Toggle modes every bar
  
  // Word melody state
  let lastWordTime = 0;
  const wordInterval = sixteenthNote * 64; // Play word every 4 bars
  let upperOctaveShift = 0; // Track relative octave shifts
  const wordCategories = ['hook', 'melodic', 'high', 'trap', 'mood'];
  let wordCategoryIndex = 0;
  
  // Get words to use - either from seed word or random
  const getNextWord = () => {
    if (wordSeed) return wordSeed;
    const category = wordCategories[wordCategoryIndex % wordCategories.length];
    wordCategoryIndex++;
    return getRandomWord(category);
  };
  
  // Octave patterns for variety - moves through musical phrases
  const octavePattern = [5, 5, 4, 5, 6, 5, 4, 4, 5, 6, 5, 5, 4, 5, 5, 6]; // 16 bar cycle
  let octaveIndex = 0;
  
  while (lastEventIndex < track.events.length) {
    const now = Date.now() - startTime;
    const currentBar = Math.floor(globalStep / 16);
    const progressRatio = globalStep / totalSteps;
    
    // Wave type changes during playback - in rhythm, no pause
    if (waveChanges && now - lastWaveChange > waveChangeInterval) {
      pressTab(client); // Fire and forget - no await
      lastWaveChange = now;
      process.stdout.write(`${CYAN}⟳${RESET}`);
    }
    
    // Word melody - play a word every 4 bars (at musical phrase boundaries)
    if (wordMode && now - lastWordTime > wordInterval && currentBar > 0) {
      const word = getNextWord();
      const wordVelocity = Math.floor(70 + progressRatio * 50); // Build velocity over time
      
      // Shift octave up for variety sometimes
      if (currentBar % 8 === 0 && upperOctaveShift < 2) {
        pressOctaveShift(client, 'up');
        upperOctaveShift++;
        process.stdout.write(`${YELLOW}>${RESET}`);
      } else if (currentBar % 8 === 4 && upperOctaveShift > 0) {
        pressOctaveShift(client, 'down');
        upperOctaveShift--;
        process.stdout.write(`${YELLOW}<${RESET}`);
      }
      
      // Play the word - fire and forget (async but don't await)
      playWordRhythmic(client, word, wordVelocity, swing > 55);
      process.stdout.write(`${GREEN}"${word}"${RESET}`);
      
      lastWordTime = now;
    }
    
    // Octave changes - every 2 bars, follow pattern
    if (now - lastOctaveChange > octaveChangeInterval) {
      const newOctave = octavePattern[octaveIndex % octavePattern.length];
      if (newOctave !== currentOctave) {
        pressNumber(client, newOctave);
        currentOctave = newOctave;
        process.stdout.write(`${YELLOW}[${newOctave}]${RESET}`);
      }
      octaveIndex++;
      lastOctaveChange = now;
    }
    
    // Mode toggling based on musical position
    if (now - lastModeChange > modeToggleInterval) {
      // Quick mode: activate during hi-hat heavy sections (every other bar, second half of track)
      const shouldQuickMode = progressRatio > 0.25 && (currentBar % 4 === 2 || currentBar % 4 === 3);
      if (shouldQuickMode !== quickModeActive) {
        pressShift(client, 'left'); // Toggle quick mode
        quickModeActive = shouldQuickMode;
        if (quickModeActive) {
          process.stdout.write(`${MAGENTA}⚡${RESET}`);
        }
      }
      
      // Slide mode: activate during melodic passages (every 4 bars, for 1 bar)
      const shouldSlideMode = (currentBar % 8 === 4 || currentBar % 8 === 5);
      if (shouldSlideMode !== slideModeActive) {
        pressShift(client, 'right'); // Toggle slide mode
        slideModeActive = shouldSlideMode;
        if (slideModeActive) {
          process.stdout.write(`${CYAN}~${RESET}`);
        }
      }
      
      lastModeChange = now;
    }
    
    while (lastEventIndex < track.events.length && track.events[lastEventIndex].time <= now) {
      const event = track.events[lastEventIndex];
      const stepInBar = globalStep % 16;
      
      // Get per-drum-type velocities from curve
      const velocities = velocityFn(stepInBar, globalStep, totalSteps);
      
      // Apply global dynamics based on position (verse/chorus feel)
      let dynamicMultiplier = 1.0;
      // Build up through track
      if (progressRatio < 0.25) dynamicMultiplier = 0.7; // Intro: quieter
      else if (progressRatio < 0.5) dynamicMultiplier = 0.85; // Verse 1
      else if (progressRatio < 0.75) dynamicMultiplier = 1.0; // Chorus: full volume
      else dynamicMultiplier = 0.9 + Math.sin(progressRatio * Math.PI * 2) * 0.1; // Outro: slight variation
      
      if (event.type === 'kick' || event.type === 'snare' || event.type === 'hat' || event.type === 'ride') {
        // Get velocity for this specific drum type
        let drumVelocity = velocities[event.type] || velocities.hat || 80;
        drumVelocity = Math.round(drumVelocity * dynamicMultiplier);
        drumVelocity = Math.max(30, Math.min(127, drumVelocity)); // Clamp
        pressDrumKey(client, event.type, drumVelocity);
        globalStep++;
      } else if (event.type === 'bass' || event.type === 'melody') {
        // Melody velocity with dynamics
        let melodyVelocity = velocities.melody || velocities.snare || 100;
        melodyVelocity = Math.round(melodyVelocity * dynamicMultiplier);
        melodyVelocity = Math.max(40, Math.min(120, melodyVelocity));
        
        // Adjust duration based on mode
        let duration = Math.min(event.duration, 300);
        if (quickModeActive) duration = Math.min(duration, 100); // Short staccato
        if (slideModeActive) duration = Math.min(event.duration, 500); // Longer, legato
        
        playNote(client, event.note, duration, melodyVelocity);
      }
      
      lastEventIndex++;
    }
    
    await new Promise(r => setTimeout(r, 5));
  }
  
  // Reset modes at end
  if (quickModeActive) releaseShift(client, 'left');
  if (slideModeActive) releaseShift(client, 'right');
  if (currentOctave !== 5) pressNumber(client, 5);
  
  // Reset octave shifts back to neutral
  while (upperOctaveShift > 0) {
    pressOctaveShift(client, 'down');
    upperOctaveShift--;
    await new Promise(r => setTimeout(r, 30));
  }
  
  await new Promise(r => setTimeout(r, 300));
  
  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`\n${GREEN}✓ ${patternName} complete (${elapsed}s)${RESET}\n`);
  
  return elapsed;
}

// Main execution
async function main() {
  console.log('\n🎤 Hip-Hop Beat Generator');
  console.log('Usage: test-hiphop [pattern] [bars=N] [bpm=N] [swing=N] [velocity] [waves] [words] [word=xyz] [room]');
  console.log('  bars: number of bars (default: 16)');
  console.log('  bpm: tempo in BPM (default: 90)');
  console.log('  pattern: boombap, trap, lofi, dilla, 808, halftime, nyc, south, gfunk, phonk, garage, minimal, afro, dembow, break');
  console.log('  scale: minor, dorian, phrygian');
  console.log('  swing: 50-80 (50=straight, 66=triplet)');
  console.log('  velocity: flat, accent, bounce, humanize, ghost, pressure, swung, crescendo, decrescendo');
  console.log('  waves: change wave type during playback');
  console.log('  words: use dictionary words as melodies');
  console.log('  word=xyz: use specific word as melody seed (e.g., word=fade, word=cage)');
  console.log('  room: enable reverb/room mode');
  console.log('  all: play through all patterns (2 bars each)');
  console.log('');
  
  const args = process.argv.slice(2);
  
  // Parse arguments
  let bars = 16;
  let bpm = 90;
  let pattern = 'boombap';
  let scale = 'minor';
  let seed = Date.now();
  let swing = 50;
  let density = 'sparse';
  let velocityCurve = 'humanize';
  let playAll = false;
  let waveChanges = false;
  let wordMode = false;
  let wordSeed = null;
  let roomMode = false;
  
  for (const arg of args) {
    if (arg.startsWith('swing=')) {
      swing = parseInt(arg.split('=')[1]) || 50;
    } else if (arg.startsWith('bpm=')) {
      bpm = parseInt(arg.split('=')[1]) || 90;
    } else if (arg.startsWith('density=')) {
      density = arg.split('=')[1] || 'sparse';
    } else if (arg.startsWith('velocity=')) {
      velocityCurve = arg.split('=')[1] || 'humanize';
    } else if (arg.startsWith('bars=')) {
      bars = parseInt(arg.split('=')[1]) || 16;
    } else if (arg.startsWith('word=')) {
      wordSeed = arg.split('=')[1] || null;
      wordMode = true;
    } else if (arg === 'all') {
      playAll = true;
    } else if (arg === 'waves') {
      waveChanges = true;
    } else if (arg === 'words') {
      wordMode = true;
    } else if (arg === 'room') {
      roomMode = true;
    } else if (!isNaN(parseInt(arg))) {
      if (bars === 16) bars = parseInt(arg);
      else if (seed === Date.now()) seed = parseInt(arg);
    } else if (BEAT_PATTERNS[arg]) {
      pattern = arg;
    } else if (SCALES[arg]) {
      scale = arg;
    } else if (HOOK_PATTERNS[arg]) {
      density = arg;
    } else if (VELOCITY_CURVES[arg]) {
      velocityCurve = arg;
    }
  }
  
  testLog('Starting Hip-Hop Beat Generator\n');
  
  try {
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await new Promise(resolve => setTimeout(resolve, 500));
    
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    // Enable console log tracking (filter for relevant messages only)
    const consoleHandler = (type, msg) => {
      // Filter out noise
      if (msg.includes('MIDI') || msg.includes('UDP') || msg.includes('Failed to fetch') ||
          msg.includes('Aesthetic') || msg.includes('WebGPU') || msg.includes('color:') ||
          msg.includes('renderer') || msg.includes('BIOS')) {
        return; // Silently ignore
      }
      // Show room/reverb messages
      if (msg.includes('🏠') || msg.includes('ROOM') || msg.includes('REVERB')) {
        const prefix = type === 'error' ? '❌' : type === 'warn' ? '⚠️' : '🔊';
        console.log(`${CYAN}${prefix} [browser] ${msg}${RESET}`);
      }
    };
    // Set up console callback BEFORE navigation so reconnects use it
    await client.enableConsole(consoleHandler);
    
    await client.jump('notepat');
    testLog('Navigated to notepat');
    
    await new Promise(r => setTimeout(r, 2000));
    
    client.close();
    await client.connect();
    testLog('Reconnected after navigation');
    
    // Re-enable console tracking after reconnect (uses stored callback)
    await client.enableConsole();
    
    await new Promise(r => setTimeout(r, 500));
    
    await client.activateAudio();
    testLog('Audio context activated\n');
    
    // Enable room/reverb mode if requested
    if (roomMode) {
      await pressRoom(client);
      testLog('🏠 Room mode enabled\n');
      await new Promise(r => setTimeout(r, 100));
    }
    
    if (playAll) {
      // Play all patterns
      console.log(`${MAGENTA}╔═══════════════════════════════════════════════════════╗${RESET}`);
      console.log(`${MAGENTA}║  🎵 PLAYING ALL PATTERNS - ${bars} bars each @ ${bpm} BPM     ${RESET}`);
      console.log(`${MAGENTA}╚═══════════════════════════════════════════════════════╝${RESET}\n`);
      
      const patterns = Object.keys(BEAT_PATTERNS);
      let totalTime = 0;
      
      for (const p of patterns) {
        const elapsed = await playPattern(client, p, bars, bpm, scale, seed + patterns.indexOf(p), swing, density, velocityCurve, waveChanges, wordMode, wordSeed);
        totalTime += parseFloat(elapsed);
        await new Promise(r => setTimeout(r, 500)); // Pause between patterns
      }
      
      console.log('═'.repeat(60));
      successLog(`All ${patterns.length} patterns complete! Total: ${totalTime.toFixed(1)}s`);
    } else {
      // Play single pattern
      await playPattern(client, pattern, bars, bpm, scale, seed, swing, density, velocityCurve, waveChanges, wordMode, wordSeed);
      console.log('═'.repeat(60) + '\n');
      console.log(`${DIM}💡 Try: test-hiphop trap words waves bpm=85${RESET}`);
      console.log(`${DIM}💡 Try: test-hiphop lofi word=fade swing=66 room${RESET}\n`);
    }
    
    // Turn off room mode if it was enabled
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

// Export main for use by artery CLI
export { main };

// Only run automatically when executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
