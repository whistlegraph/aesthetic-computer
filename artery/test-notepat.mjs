#!/usr/bin/env node
/**
 * 🎹 Notepat Composition Tester
 * 
 * The definitive test suite for notepat - combines:
 * - Trap waltz (3/4 time)
 * - Hip-hop beats (4/4 time)
 * - All notepat features: room/reverb, wave types, quick mode, slide mode, octaves
 * 
 * Usage: node test-notepat.mjs [genre] [style] [options]
 */

import Artery from './artery.mjs';
import { noteToKey } from '../.vscode/tests/melodies.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';
const BOLD = '\x1b[1m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const playingLog = (msg) => console.log(`${YELLOW}🎵 ${msg}${RESET}`);

// ═══════════════════════════════════════════════════════════════════════════
// DRUM KEY MAPPINGS (from notepat.mjs)
// ═══════════════════════════════════════════════════════════════════════════
const DRUM_KEYS = {
  'kick': { key: 'ArrowDown', code: 'ArrowDown', keyCode: 40 },
  'snare': { key: ' ', code: 'Space', keyCode: 32 },
  'hat': { key: 'ArrowUp', code: 'ArrowUp', keyCode: 38 },
  'hatOpen': { key: 'Alt', code: 'AltRight', keyCode: 18 },
  'tomLow': { key: 'ArrowLeft', code: 'ArrowLeft', keyCode: 37 },
  'tomHigh': { key: 'ArrowRight', code: 'ArrowRight', keyCode: 39 },
};

// ═══════════════════════════════════════════════════════════════════════════
// BEAT PATTERNS
// ═══════════════════════════════════════════════════════════════════════════

// 4/4 Hip-Hop patterns (16 steps per bar)
const HIPHOP_PATTERNS = {
  'trap': {
    kick:    [1,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
  'boombap': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,0,1, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
  },
  'lofi': {
    kick:  [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,1],
    hat:   [0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0],
  },
  '808': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,1,0, 0,0,0,0],
    snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:   [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,1],
  },
  'halftime': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
  },
  'phonk': {
    kick:    [1,0,0,0, 0,0,1,0, 0,1,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,1],
    hat:     [1,0,1,1, 1,0,1,1, 1,0,1,1, 1,0,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,1],
  },
  'drill': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
  'gfunk': {
    kick:  [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0],
    snare: [0,0,0,0, 1,0,0,1, 0,0,0,0, 1,0,0,0],
    hat:   [1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1],
  },
};

// 3/4 Waltz patterns (12 steps per bar)
const WALTZ_PATTERNS = {
  'classic': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,0],
  },
  'dark': {
    kick:    [1,0,0,0, 0,0,1,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
  'dreamy': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,0],
    hat:     [1,0,1,0, 1,0,1,0, 1,0,1,0],
    hatOpen: [0,0,0,0, 0,0,0,0, 0,0,0,1],
  },
  'baroque': {
    kick:    [1,0,0,1, 0,0,0,0, 1,0,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,1,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,0, 0,0,1,0, 0,0,0,1],
  },
  'minimal': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [0,0,0,0, 1,0,0,0, 0,0,0,0],
  },
  'phonk': {
    kick:    [1,0,0,0, 0,0,1,0, 0,1,0,0],
    snare:   [0,0,0,0, 1,0,0,0, 0,0,0,1],
    hat:     [1,0,1,1, 1,0,1,1, 1,0,1,1],
    hatOpen: [0,0,0,0, 0,0,0,1, 0,0,0,1],
  },
  'viennese': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,0,0],
    snare:   [0,0,0,0, 1,0,1,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
  },
  'drill': {
    kick:    [1,0,0,0, 0,0,0,0, 0,0,1,0],
    snare:   [0,0,0,0, 0,0,0,0, 1,0,0,0],
    hat:     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    hatOpen: [0,0,0,1, 0,0,0,1, 0,0,0,1],
  },
};

// ═══════════════════════════════════════════════════════════════════════════
// SCALES & PROGRESSIONS
// ═══════════════════════════════════════════════════════════════════════════
const SCALES = {
  'minor': [0, 2, 3, 5, 7, 8, 10],
  'dorian': [0, 2, 3, 5, 7, 9, 10],
  'phrygian': [0, 1, 3, 5, 7, 8, 10],
  'harmonic': [0, 2, 3, 5, 7, 8, 11],
  'major': [0, 2, 4, 5, 7, 9, 11],
};

const PROGRESSIONS = {
  'classical': [0, 3, 4, 4],
  'dark': [0, 5, 6, 4],
  'romantic': [0, 3, 4, 0],
  'trap': [0, 6, 3, 4],
};

// ═══════════════════════════════════════════════════════════════════════════
// WAVE TYPES (cycled with Tab)
// ═══════════════════════════════════════════════════════════════════════════
const WAVE_TYPES = ['sine', 'triangle', 'sawtooth', 'square'];

// ═══════════════════════════════════════════════════════════════════════════
// KEY DISPATCH FUNCTIONS
// ═══════════════════════════════════════════════════════════════════════════

// Press drum key using CDP
async function pressDrumKey(client, drumType, velocity = 100) {
  const drum = DRUM_KEYS[drumType];
  if (!drum) return;
  
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: drum.key,
    code: drum.code,
    windowsVirtualKeyCode: drum.keyCode,
  });
  
  setTimeout(async () => {
    await client.send('Input.dispatchKeyEvent', {
      type: 'keyUp', 
      key: drum.key,
      code: drum.code,
      windowsVirtualKeyCode: drum.keyCode,
    });
  }, 30);
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

// Play note with duration
async function playNote(client, note, duration, velocity = 100) {
  const key = noteToKey(note, 5);
  if (!key) return;
  
  await pressNoteKey(client, key, velocity);
  await new Promise(r => setTimeout(r, Math.min(duration, 200)));
  await releaseNoteKey(client, key);
}

// Press Tab to cycle wave type
async function pressTab(client) {
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: 'Tab',
    code: 'Tab',
    windowsVirtualKeyCode: 9,
  });
  await new Promise(r => setTimeout(r, 30));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key: 'Tab',
    code: 'Tab',
    windowsVirtualKeyCode: 9,
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
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: String(num),
    code: `Digit${num}`,
    windowsVirtualKeyCode: 48 + num,
  });
  await new Promise(r => setTimeout(r, 30));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key: String(num),
    code: `Digit${num}`,
    windowsVirtualKeyCode: 48 + num,
  });
}

// Press < (comma) or > (period) for relative octave shifts
async function pressOctaveShift(client, direction) {
  const key = direction === 'up' ? '.' : ',';
  const code = direction === 'up' ? 'Period' : 'Comma';
  const keyCode = direction === 'up' ? 190 : 188;
  
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key,
    code,
    windowsVirtualKeyCode: keyCode,
  });
  await new Promise(r => setTimeout(r, 30));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key,
    code,
    windowsVirtualKeyCode: keyCode,
  });
}

// Room/reverb toggle
async function pressRoom(client) {
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: '/',
    code: 'Slash',
    windowsVirtualKeyCode: 191,
  });
  await new Promise(r => setTimeout(r, 50));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp', 
    key: '/',
    code: 'Slash',
    windowsVirtualKeyCode: 191,
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// SCALE/NOTE HELPERS
// ═══════════════════════════════════════════════════════════════════════════

function getScaleNote(degree, scale = 'minor', octave = 4) {
  const scaleIntervals = SCALES[scale];
  const noteIndex = ((degree % 7) + 7) % 7;
  const semitones = scaleIntervals[noteIndex];
  
  const semitoneToNote = {
    0: 'c', 2: 'd', 3: 'e', 4: 'e', 5: 'f', 7: 'g', 8: 'a', 9: 'a', 10: 'b', 11: 'b'
  };
  
  const note = semitoneToNote[semitones] || 'c';
  return { note, octave };
}

// ═══════════════════════════════════════════════════════════════════════════
// COMPOSITION GENERATOR
// ═══════════════════════════════════════════════════════════════════════════

function generateComposition(opts = {}) {
  const {
    bars = 8,
    bpm = 140,
    genre = 'waltz', // 'waltz' (3/4) or 'hiphop' (4/4)
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
  
  const isWaltz = genre === 'waltz';
  const stepsPerBar = isWaltz ? 12 : 16;
  const beatsPerBar = isWaltz ? 3 : 4;
  
  const patterns = isWaltz ? WALTZ_PATTERNS : HIPHOP_PATTERNS;
  const drumPattern = patterns[style] || patterns[Object.keys(patterns)[0]];
  const chordProg = PROGRESSIONS[progression] || [0, 3, 4, 0];
  
  const events = [];
  const msPerBeat = 60000 / bpm;
  const msPerBar = msPerBeat * beatsPerBar;
  const msPerStep = msPerBeat / 4;
  
  for (let bar = 0; bar < bars; bar++) {
    const barStart = bar * msPerBar;
    const chordIndex = bar % chordProg.length;
    const chordDegree = chordProg[chordIndex];
    const phrasePosition = bar % 4; // 4-bar phrase structure
    
    // DRUMS
    for (let step = 0; step < stepsPerBar; step++) {
      const stepTime = barStart + (step * msPerStep);
      
      if (drumPattern.kick?.[step]) {
        events.push({
          time: stepTime,
          type: 'kick',
          velocity: step === 0 ? 127 : 100,
        });
      }
      
      if (drumPattern.snare?.[step]) {
        events.push({
          time: stepTime,
          type: 'snare',
          velocity: 100,
        });
      }
      
      if (drumPattern.hat?.[step]) {
        const accent = (step % 4 === 0) ? 100 : Math.floor(50 + seededRandom() * 40);
        events.push({
          time: stepTime,
          type: 'hat',
          velocity: accent,
        });
      }
      
      if (drumPattern.hatOpen?.[step]) {
        events.push({
          time: stepTime,
          type: 'hatOpen',
          velocity: 90,
        });
      }
    }
    
    // ═══════════════════════════════════════════════════════════════════
    // LEFT HAND - Bass + Chord Accompaniment (Octaves 3-4)
    // "OOM-pah-pah" waltz pattern or root-based hip-hop
    // ═══════════════════════════════════════════════════════════════════
    
    // Beat 1 - Root bass note (the deep "OOM")
    const bassNote = getScaleNote(chordDegree, scale, 3);
    events.push({
      time: barStart,
      type: 'melody',
      note: bassNote.note,
      octave: bassNote.octave,
      velocity: 110,
      duration: msPerBeat * 0.9,
    });
    
    if (isWaltz) {
      // Beat 2 - First "PAH" - chord 5th in mid register
      if (seededRandom() < 0.85) {
        const pah1Note = getScaleNote(chordDegree + 4, scale, 4);
        events.push({
          time: barStart + msPerBeat,
          type: 'melody',
          note: pah1Note.note,
          octave: pah1Note.octave,
          velocity: 70,
          duration: msPerBeat * 0.4,
        });
      }
      
      // Beat 3 - Second "PAH" - chord 3rd in mid register
      if (seededRandom() < 0.8) {
        const pah2Note = getScaleNote(chordDegree + 2, scale, 4);
        events.push({
          time: barStart + msPerBeat * 2,
          type: 'melody',
          note: pah2Note.note,
          octave: pah2Note.octave,
          velocity: 65,
          duration: msPerBeat * 0.4,
        });
      }
      
      // Occasional sub-bass octave doubling for power
      if (seededRandom() < 0.3 && phrasePosition === 0) {
        const subBass = getScaleNote(chordDegree, scale, 2);
        events.push({
          time: barStart,
          type: 'melody',
          note: subBass.note,
          octave: subBass.octave,
          velocity: 80,
          duration: msPerBeat * 0.5,
        });
      }
      
      // Add chord voicing - stacked 3rds on some bars
      if (seededRandom() < 0.4 && bar > 1) {
        const chordTime = barStart + msPerBeat * 1.5;
        // Root + 3rd + 5th in octave 4
        [0, 2, 4].forEach((interval, i) => {
          const chordTone = getScaleNote(chordDegree + interval, scale, 4);
          events.push({
            time: chordTime + (i * 15), // Slight spread for arpeggiated feel
            type: 'melody',
            note: chordTone.note,
            octave: chordTone.octave,
            velocity: 55 - i * 5,
            duration: msPerBeat * 0.3,
          });
        });
      }
    } else {
      // Hip-hop: sustained bass or rhythmic pattern
      if (seededRandom() < 0.5 && bar % 2 === 1) {
        const bass5th = getScaleNote(chordDegree + 4, scale, 3);
        events.push({
          time: barStart + msPerBeat * 2,
          type: 'melody',
          note: bass5th.note,
          octave: bass5th.octave,
          velocity: 95,
          duration: msPerBeat * 0.6,
        });
      }
    }
    
    // ═══════════════════════════════════════════════════════════════════
    // RIGHT HAND - Melodic Lines (Octaves 5-6)
    // Flowing phrases, ornaments, expressive topline
    // ═══════════════════════════════════════════════════════════════════
    
    if (isWaltz || seededRandom() < 0.7) {
      const melodyOctave = 5 + (seededRandom() < 0.3 ? 1 : 0); // Sometimes go to octave 6
      
      // Beat 1 - Strong downbeat melody note
      const beat1Degrees = [chordDegree, chordDegree + 4, chordDegree + 2, chordDegree + 7];
      const beat1Degree = beat1Degrees[Math.floor(seededRandom() * beat1Degrees.length)];
      const beat1Note = getScaleNote(beat1Degree, scale, melodyOctave);
      events.push({
        time: barStart + 10, // Slight delay after bass for separation
        type: 'melody',
        note: beat1Note.note,
        octave: beat1Note.octave,
        velocity: 90,
        duration: msPerBeat * 0.85,
      });
      
      // Beat 2 - Stepwise motion or leap
      if (seededRandom() < 0.8) {
        const isLeap = seededRandom() < 0.25;
        const beat2Direction = seededRandom() < 0.5 ? 1 : -1;
        const beat2Interval = isLeap ? (beat2Direction * 3) : beat2Direction;
        const beat2Degree = beat1Degree + beat2Interval;
        const beat2Note = getScaleNote(beat2Degree, scale, melodyOctave);
        events.push({
          time: barStart + msPerBeat + 10,
          type: 'melody',
          note: beat2Note.note,
          octave: beat2Note.octave,
          velocity: 72,
          duration: msPerBeat * 0.5,
        });
        
        // Occasional passing tone between beats 2 and 3
        if (seededRandom() < 0.4) {
          const passingDegree = beat2Degree + (seededRandom() < 0.5 ? 1 : -1);
          const passingNote = getScaleNote(passingDegree, scale, melodyOctave);
          events.push({
            time: barStart + msPerBeat * 1.5 + 10,
            type: 'melody',
            note: passingNote.note,
            octave: passingNote.octave,
            velocity: 55,
            duration: msPerStep * 0.8,
          });
        }
      }
      
      // Beat 3 - Resolution or continuation
      if (seededRandom() < 0.75) {
        const isPhrasEnd = phrasePosition === 3;
        // On phrase endings, resolve to strong chord tone
        const beat3Degree = isPhrasEnd 
          ? chordDegree + (seededRandom() < 0.6 ? 0 : 4) // Root or 5th
          : beat1Degree + Math.floor(seededRandom() * 4) - 2;
        const beat3Octave = isPhrasEnd ? 5 : melodyOctave; // Resolve to stable octave
        const beat3Note = getScaleNote(beat3Degree, scale, beat3Octave);
        events.push({
          time: barStart + msPerBeat * 2 + 10,
          type: 'melody',
          note: beat3Note.note,
          octave: beat3Note.octave,
          velocity: isPhrasEnd ? 88 : 70,
          duration: msPerBeat * (isPhrasEnd ? 0.9 : 0.55),
        });
      }
      
      // Melodic ornaments - 16th note runs on non-phrase-ending bars
      if (seededRandom() < 0.35 && phrasePosition !== 3) {
        const ornamentTime = barStart + msPerBeat * 2.5;
        const runDirection = seededRandom() < 0.5 ? 1 : -1;
        const runLength = 2 + Math.floor(seededRandom() * 2); // 2-3 notes
        for (let i = 0; i < runLength; i++) {
          const ornDegree = chordDegree + 3 + (i * runDirection);
          const ornNote = getScaleNote(ornDegree, scale, melodyOctave);
          events.push({
            time: ornamentTime + (i * msPerStep),
            type: 'melody',
            note: ornNote.note,
            octave: ornNote.octave,
            velocity: 58 - (i * 3), // Diminuendo
            duration: msPerStep * 0.75,
          });
        }
      }
      
      // High register grace notes / trills
      if (seededRandom() < 0.2 && bar > 2) {
        const graceTime = barStart + msPerStep * 0.5;
        const graceDegree = beat1Degree + 1;
        const graceNote = getScaleNote(graceDegree, scale, 6);
        events.push({
          time: graceTime,
          type: 'melody',
          note: graceNote.note,
          octave: graceNote.octave,
          velocity: 50,
          duration: msPerStep * 0.3,
        });
      }
    }
    
    // Counter-melody (mid-right hand, octave 5) - adds depth
    if (seededRandom() < 0.3 && bar > 0 && phrasePosition !== 3) {
      const counterDegree = chordDegree + (seededRandom() < 0.5 ? 2 : 5);
      const counterNote = getScaleNote(counterDegree, scale, 5);
      events.push({
        time: barStart + msPerBeat * 1.25,
        type: 'melody',
        note: counterNote.note,
        octave: counterNote.octave,
        velocity: 50,
        duration: msPerBeat * 0.4,
      });
    }
  }
  
  events.sort((a, b) => a.time - b.time);
  
  return {
    events,
    totalDuration: bars * msPerBar,
    bpm,
    bars,
    genre,
    style,
    scale,
    progression,
    stepsPerBar,
    beatsPerBar,
    drumPattern,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// PLAYBACK ENGINE
// ═══════════════════════════════════════════════════════════════════════════

async function playComposition(client, comp, options = {}) {
  const {
    waveChanges = false,
    quickMode = false,
    slideMode = false,
    octaveChanges = false,
    roomMode = false, // Track room mode for rhythmic toggling
  } = options;
  
  const { events, totalDuration, bpm, bars, genre, style, scale, progression, stepsPerBar, drumPattern } = comp;
  const isWaltz = genre === 'waltz';
  
  console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
  console.log(`${YELLOW}${isWaltz ? '🎭 TRAP WALTZ' : '🎤 HIP-HOP'}: ${style.toUpperCase()} | ${bpm} BPM | ${scale} | ${bars} bars${RESET}`);
  console.log(`${DIM}   Time: ${isWaltz ? '3/4' : '4/4'} | Progression: ${progression}${RESET}`);
  
  // Show enabled features
  const features = [];
  if (roomMode) features.push('🏠 room');
  if (waveChanges) features.push('⟳ waves');
  if (quickMode) features.push('⚡ quick');
  if (slideMode) features.push('~ slide');
  if (octaveChanges) features.push('🎹 octaves');
  if (features.length) {
    console.log(`${DIM}   Features: ${features.join(' | ')}${RESET}`);
  }
  
  // Show pattern
  console.log(`\n${DIM}Pattern (${stepsPerBar} steps per bar):${RESET}`);
  if (drumPattern.kick) console.log(`  Kick:  ${drumPattern.kick.map(x => x ? '●' : '○').join('')}`);
  if (drumPattern.snare) console.log(`  Snare: ${drumPattern.snare.map(x => x ? '●' : '○').join('')}`);
  if (drumPattern.hat) console.log(`  Hat:   ${drumPattern.hat.map(x => x ? '●' : '○').join('')}`);
  if (drumPattern.hatOpen?.some(x => x)) console.log(`  Open:  ${drumPattern.hatOpen.map(x => x ? '●' : '○').join('')}`);
  
  playingLog(`Now playing: ${bars} bars at ${bpm} BPM\n`);
  
  const startTime = Date.now();
  let eventIndex = 0;
  let lastBar = -1;
  const msPerBar = (60000 / bpm) * (isWaltz ? 3 : 4);
  const msPerStep = (60000 / bpm) / 4;
  
  // Feature state
  let lastWaveChange = 0;
  let lastOctaveChange = 0;
  let lastModeCheck = 0;
  let lastRoomToggle = 0;
  let currentOctave = 5;
  let quickModeActive = false;
  let slideModeActive = false;
  let roomActive = roomMode; // Start with initial room state
  const waveChangeInterval = msPerStep * 16; // Every bar
  const octaveChangeInterval = msPerStep * 32; // Every 2 bars
  const modeCheckInterval = msPerStep * 8; // Check every half bar
  const roomToggleInterval = msPerStep * 32; // Toggle room every 2 bars
  const octavePattern = [5, 5, 4, 5, 6, 5, 4, 4];
  let octaveIndex = 0;
  
  // Visual symbols
  const symbols = isWaltz 
    ? ['🎭', '💃', '🕺', '🌙', '✨', '🎪', '🦇', '🌹']
    : ['🎤', '🔥', '💎', '🎧', '⚡', '🌊', '🎯', '💫'];
  
  return new Promise((resolve) => {
    const interval = setInterval(async () => {
      const elapsed = Date.now() - startTime;
      const currentBar = Math.floor(elapsed / msPerBar);
      const progressRatio = currentBar / bars;
      
      // Bar indicator
      if (currentBar !== lastBar && currentBar < bars) {
        const symbol = symbols[currentBar % symbols.length];
        process.stdout.write(symbol);
        lastBar = currentBar;
      }
      
      // Wave type changes - every bar
      if (waveChanges && elapsed - lastWaveChange > waveChangeInterval) {
        pressTab(client);
        lastWaveChange = elapsed;
        process.stdout.write(`${CYAN}⟳${RESET}`);
      }
      
      // Rhythmic room toggle - every 2 bars after the first few
      if (roomMode && currentBar >= 4 && elapsed - lastRoomToggle > roomToggleInterval) {
        // Toggle room on/off rhythmically
        const shouldRoom = (Math.floor(currentBar / 2) % 2) === 0;
        if (shouldRoom !== roomActive) {
          pressRoom(client);
          roomActive = shouldRoom;
          process.stdout.write(roomActive ? `${CYAN}🏠${RESET}` : `${DIM}○${RESET}`);
        }
        lastRoomToggle = elapsed;
      }
      
      // Octave changes - every 2 bars
      if (octaveChanges && elapsed - lastOctaveChange > octaveChangeInterval) {
        const newOctave = octavePattern[octaveIndex % octavePattern.length];
        if (newOctave !== currentOctave) {
          pressNumber(client, newOctave);
          currentOctave = newOctave;
          process.stdout.write(`${YELLOW}[${newOctave}]${RESET}`);
        }
        octaveIndex++;
        lastOctaveChange = elapsed;
      }
      
      // Mode toggling - check every half bar
      if (elapsed - lastModeCheck > modeCheckInterval) {
        // Quick mode - toggle on bars 2-3 of every 4-bar phrase
        if (quickMode) {
          const shouldQuick = (currentBar % 4 === 2) || (currentBar % 4 === 3);
          if (shouldQuick !== quickModeActive) {
            pressShift(client, 'left');
            quickModeActive = shouldQuick;
            process.stdout.write(quickModeActive ? `${MAGENTA}⚡${RESET}` : `${DIM}•${RESET}`);
          }
        }
        
        // Slide mode - toggle on bars 4-5 of every 8-bar phrase  
        if (slideMode) {
          const barIn8 = currentBar % 8;
          const shouldSlide = (barIn8 === 4) || (barIn8 === 5);
          if (shouldSlide !== slideModeActive) {
            pressShift(client, 'right');
            slideModeActive = shouldSlide;
            process.stdout.write(slideModeActive ? `${CYAN}~${RESET}` : `${DIM}•${RESET}`);
          }
        }
        
        lastModeCheck = elapsed;
      }
      
      // Play events
      while (eventIndex < events.length && events[eventIndex].time <= elapsed) {
        const event = events[eventIndex];
        
        if (['kick', 'snare', 'hat', 'hatOpen'].includes(event.type)) {
          await pressDrumKey(client, event.type, event.velocity);
        } else if (event.type === 'melody') {
          const key = noteToKey(event.note, event.octave);
          if (key) {
            await pressNoteKey(client, key, event.velocity);
            setTimeout(async () => {
              await releaseNoteKey(client, key);
            }, event.duration || 200);
          }
        }
        
        eventIndex++;
      }
      
      // Check if done
      if (elapsed >= totalDuration) {
        clearInterval(interval);
        
        // Clean up modes
        if (quickModeActive) pressShift(client, 'left');
        if (slideModeActive) pressShift(client, 'right');
        
        const actualTime = ((Date.now() - startTime) / 1000).toFixed(1);
        console.log(`\n${GREEN}✓ ${style} complete (${actualTime}s)${RESET}`);
        resolve(parseFloat(actualTime));
      }
    }, 5);
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  const args = process.argv.slice(2);
  
  // Defaults
  let genre = 'waltz';
  let style = 'classic';
  let bars = 8;
  let bpm = 140;
  let scale = 'minor';
  let seed = Date.now();
  let progression = 'classical';
  let roomMode = false;
  let waveChanges = false;
  let quickMode = false;
  let slideMode = false;
  let octaveChanges = false;
  let playAll = false;
  let suiteMode = false;
  
  // Help
  console.log(`\n${BOLD}${MAGENTA}🎹 Notepat Composition Tester${RESET}`);
  console.log(`${DIM}Usage: test-notepat [genre] [style] [options]${RESET}\n`);
  console.log(`${DIM}Genres:${RESET}`);
  console.log(`  ${CYAN}waltz${RESET}   - 3/4 time (${Object.keys(WALTZ_PATTERNS).join(', ')})`);
  console.log(`  ${CYAN}hiphop${RESET}  - 4/4 time (${Object.keys(HIPHOP_PATTERNS).join(', ')})`);
  console.log(`\n${DIM}Options:${RESET}`);
  console.log(`  ${YELLOW}room${RESET}      - Enable reverb`);
  console.log(`  ${YELLOW}waves${RESET}     - Cycle wave types during playback`);
  console.log(`  ${YELLOW}quick${RESET}     - Toggle quick mode on certain sections`);
  console.log(`  ${YELLOW}slide${RESET}     - Toggle slide mode on phrase boundaries`);
  console.log(`  ${YELLOW}octaves${RESET}   - Change octaves throughout`);
  console.log(`  ${YELLOW}all${RESET}       - Enable all features`);
  console.log(`  ${YELLOW}suite${RESET}     - Play all styles (16 bars each)`);
  console.log(`  ${YELLOW}bars=N${RESET}    - Number of bars (default: 8)`);
  console.log(`  ${YELLOW}bpm=N${RESET}     - Tempo (default: 140)`);
  console.log(`\n`);
  
  // Parse args
  for (const arg of args) {
    if (arg.startsWith('bars=')) bars = parseInt(arg.split('=')[1]) || 8;
    else if (arg.startsWith('bpm=')) bpm = parseInt(arg.split('=')[1]) || 140;
    else if (arg.startsWith('scale=')) scale = arg.split('=')[1] || 'minor';
    else if (arg.startsWith('progression=')) progression = arg.split('=')[1] || 'classical';
    else if (arg.startsWith('seed=')) seed = parseInt(arg.split('=')[1]) || Date.now();
    else if (arg === 'room') roomMode = true;
    else if (arg === 'waves') waveChanges = true;
    else if (arg === 'quick') quickMode = true;
    else if (arg === 'slide') slideMode = true;
    else if (arg === 'octaves') octaveChanges = true;
    else if (arg === 'all') {
      roomMode = waveChanges = quickMode = slideMode = octaveChanges = true;
    }
    else if (arg === 'suite') {
      suiteMode = true;
      bars = 16;
    }
    else if (arg === 'waltz' || arg === 'hiphop') genre = arg;
    else if (WALTZ_PATTERNS[arg]) { genre = 'waltz'; style = arg; }
    else if (HIPHOP_PATTERNS[arg]) { genre = 'hiphop'; style = arg; }
    else if (SCALES[arg]) scale = arg;
  }
  
  testLog('Starting Notepat Composition Tester\n');
  
  try {
    await Artery.openPanelStandalone();
    await new Promise(r => setTimeout(r, 1500));
    
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    await client.enableConsole((type, msg) => {
      if (msg.includes('🏠') || msg.includes('ROOM') || msg.includes('wave')) {
        console.log(`${CYAN}🔊 ${msg}${RESET}`);
      }
    });
    
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
    
    // Don't enable room here - let playComposition handle it rhythmically if roomMode is on
    // But we need to start with room ON if requested
    if (roomMode) {
      await pressRoom(client);
      testLog('🏠 Room mode enabled (will toggle rhythmically)\n');
      await new Promise(r => setTimeout(r, 100));
    }
    
    const playOptions = { waveChanges, quickMode, slideMode, octaveChanges, roomMode };
    
    if (suiteMode) {
      const patterns = genre === 'waltz' ? WALTZ_PATTERNS : HIPHOP_PATTERNS;
      const styles = Object.keys(patterns);
      
      console.log(`${MAGENTA}╔════════════════════════════════════════════════════════════╗${RESET}`);
      console.log(`${MAGENTA}║  🎹 NOTEPAT ${genre.toUpperCase()} SUITE - ${styles.length} STYLES × 16 BARS              ║${RESET}`);
      console.log(`${MAGENTA}╚════════════════════════════════════════════════════════════╝${RESET}\n`);
      
      let totalTime = 0;
      
      for (const s of styles) {
        const comp = generateComposition({
          bars: 16,
          bpm,
          genre,
          style: s,
          scale,
          seed: seed + styles.indexOf(s),
          progression,
        });
        const elapsed = await playComposition(client, comp, playOptions);
        totalTime += elapsed;
        await new Promise(r => setTimeout(r, 2000));
      }
      
      console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}`);
      console.log(`${GREEN}✅ Suite complete! Total: ${(totalTime / 60).toFixed(1)} minutes${RESET}`);
      console.log(`${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
      
    } else {
      const comp = generateComposition({ bars, bpm, genre, style, scale, seed, progression });
      await playComposition(client, comp, playOptions);
    }
    
    console.log(`\n${MAGENTA}════════════════════════════════════════════════════════════${RESET}\n`);
    console.log(`${DIM}💡 Examples:${RESET}`);
    console.log(`${DIM}   test-notepat waltz dark room waves${RESET}`);
    console.log(`${DIM}   test-notepat hiphop trap all bpm=120${RESET}`);
    console.log(`${DIM}   test-notepat waltz suite room${RESET}`);
    console.log(`${DIM}   test-notepat hiphop suite all${RESET}\n`);
    
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
