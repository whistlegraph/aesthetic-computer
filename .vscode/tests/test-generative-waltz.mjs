#!/usr/bin/env node
/**
 * Generative Waltz Composer & Player
 * 
 * Algorithmically generates waltzes with proper harmony and structure
 */

import Artery from '../artery.mjs';
import { noteToKey } from './melodies.mjs';

const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const WHITE_TEXT = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🧪${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const playingLog = (msg) => console.log(`${YELLOW}🎵 ${msg}${RESET}`);

// Musical theory for generative composition
const SCALES = {
  'major': [0, 2, 4, 5, 7, 9, 11], // C major: C D E F G A B
  'minor': [0, 2, 3, 5, 7, 8, 10], // C minor: C D Eb F G Ab Bb
  'dorian': [0, 2, 3, 5, 7, 9, 10], // Dorian mode
};

const CHORD_PROGRESSIONS = [
  [0, 3, 4, 0], // I - IV - V - I (classic)
  [0, 5, 3, 4], // I - vi - IV - V (pop progression)
  [0, 4, 5, 0], // I - V - vi - I
  [0, 2, 4, 0], // I - iii - V - I
  [0, 3, 0, 4], // I - IV - I - V
];

const NOTE_NAMES = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];

// Get note from scale degree
function getScaleNote(degree, scale = 'major', octave = 4) {
  const scaleIntervals = SCALES[scale];
  const noteIndex = degree % 7;
  const octaveShift = Math.floor(degree / 7);
  const semitones = scaleIntervals[noteIndex];
  const chromaticIndex = semitones % 12;
  
  // Map semitones to note names (C major equivalents)
  const chromaticToNote = [
    'c', null, 'd', null, 'e', 'f', null, 'g', null, 'a', null, 'b'
  ];
  
  const note = chromaticToNote[chromaticIndex];
  return note ? { note, octave: octave + octaveShift } : null;
}

// Generate a triad chord
function generateChord(rootDegree, scale = 'major', octave = 4) {
  const root = getScaleNote(rootDegree, scale, octave);
  const third = getScaleNote(rootDegree + 2, scale, octave);
  const fifth = getScaleNote(rootDegree + 4, scale, octave);
  
  return [root, third, fifth].filter(n => n !== null);
}

// Generate melodic line based on chord
function generateMelody(chordDegree, scale = 'major', style = 'flowing') {
  const melodyNotes = [];
  const baseOctave = 5;
  
  if (style === 'flowing') {
    // Smooth, stepwise motion around chord tones
    const degrees = [
      chordDegree + 4,      // Start on 5th
      chordDegree + 2,      // Move to 3rd
      chordDegree,          // Resolve to root
    ];
    degrees.forEach(deg => {
      const note = getScaleNote(deg, scale, baseOctave);
      if (note) melodyNotes.push(note);
    });
  } else if (style === 'arpeggio') {
    // Broken chord pattern
    const degrees = [chordDegree, chordDegree + 2, chordDegree + 4];
    degrees.forEach(deg => {
      const note = getScaleNote(deg, scale, baseOctave);
      if (note) melodyNotes.push(note);
    });
  } else if (style === 'leap') {
    // Larger intervals, more dramatic
    const degrees = [
      chordDegree + 4,      // Jump to 5th
      chordDegree + 6,      // Jump up
      chordDegree + 2,      // Back down to 3rd
    ];
    degrees.forEach(deg => {
      const note = getScaleNote(deg, scale, baseOctave);
      if (note) melodyNotes.push(note);
    });
  }
  
  return melodyNotes;
}

// Generate "top line" - ornamental melody above the main melody
// More frolicking and playful!
function generateTopLine(chordDegree, scale = 'major', beat = 1, seededRandom) {
  const baseOctave = 6; // Higher octave for ornamentation
  
  // Much more frequent ornaments for a frolicking feel!
  const rand = seededRandom();
  
  if (beat === 1) {
    // Strong beat: leap up for excitement
    if (rand > 0.3) {
      const degree = chordDegree + 5 + Math.floor(seededRandom() * 3); // Higher leaps!
      const note = getScaleNote(degree, scale, baseOctave);
      return note;
    }
  } else if (beat === 2) {
    // Second beat: playful passing tones
    if (rand > 0.4) {
      const degree = chordDegree + 3 + Math.floor(seededRandom() * 2);
      const note = getScaleNote(degree, scale, baseOctave);
      return note;
    }
  } else if (beat === 3) {
    // Third beat: dance-like turns and trills
    if (rand > 0.5) {
      const degree = chordDegree + 4 + Math.floor(seededRandom() * 4); // Wide range for playfulness
      const note = getScaleNote(degree, scale, baseOctave);
      return note;
    }
  }
  
  return null; // No ornament on this beat
}

// Movement characters for frolic mode - each creates a different "world"
const MOVEMENTS = [
  { name: 'Dreamy', color: '\x1b[94m', scaleProb: 0.1, intervalProb: 0.25, holdProb: 0.4, register: 5, density: 'sparse' },
  { name: 'Active', color: '\x1b[93m', scaleProb: 0.3, intervalProb: 0.1, holdProb: 0.1, register: 5, density: 'busy' },
  { name: 'Lyrical', color: '\x1b[95m', scaleProb: 0.15, intervalProb: 0.3, holdProb: 0.3, register: 5, density: 'medium' },
  { name: 'Playful', color: '\x1b[92m', scaleProb: 0.25, intervalProb: 0.15, holdProb: 0.15, register: 6, density: 'busy' },
  { name: 'Contemplative', color: '\x1b[96m', scaleProb: 0.05, intervalProb: 0.35, holdProb: 0.5, register: 4, density: 'sparse' },
  { name: 'Agitated', color: '\x1b[91m', scaleProb: 0.35, intervalProb: 0.05, holdProb: 0.05, register: 6, density: 'busy' },
  { name: 'Wistful', color: '\x1b[97m', scaleProb: 0.1, intervalProb: 0.2, holdProb: 0.35, register: 5, density: 'medium' },
  { name: 'Dancing', color: '\x1b[33m', scaleProb: 0.2, intervalProb: 0.1, holdProb: 0.2, register: 5, density: 'medium' },
];

// Generate frolic melody events for one beat
// Returns array of {note, duration, isInterval} objects
// The melody plays alongside the bass with scales, skips, and wandering
function generateFrolicEvents(chordDegree, scale, beat, seededRandom, lastNote = null, beatDuration, movement = null) {
  const baseOctave = movement?.register || 5;
  const events = [];
  const rand = seededRandom();
  
  // Movement affects probabilities
  const scaleProb = movement?.scaleProb || 0.2;
  const intervalProb = movement?.intervalProb || 0.18;
  const holdProb = movement?.holdProb || 0.15;
  const density = movement?.density || 'medium';
  
  // Start from last note for continuity
  let currentDegree = lastNote?.degree ?? (chordDegree + 2 + Math.floor(seededRandom() * 3));
  
  // Decide the rhythm for this beat based on movement density
  let rhythmPattern;
  const rhythmRand = seededRandom();
  
  if (density === 'sparse') {
    // Sparse: mostly long notes
    if (rhythmRand < 0.4) {
      rhythmPattern = [1.0]; // Quarter
    } else if (rhythmRand < 0.7) {
      rhythmPattern = [0.5, 0.5]; // Two eighths
    } else if (rhythmRand < 0.85) {
      rhythmPattern = [0.75, 0.25]; // Dotted
    } else {
      rhythmPattern = [0.5, 0.25, 0.25];
    }
  } else if (density === 'busy') {
    // Busy: lots of short notes and scales
    if (rhythmRand < 0.25) {
      rhythmPattern = [0.25, 0.25, 0.25, 0.25];
    } else if (rhythmRand < 0.45) {
      rhythmPattern = [0.167, 0.167, 0.167, 0.167, 0.167, 0.165]; // Sextuplet
    } else if (rhythmRand < 0.60) {
      rhythmPattern = [0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125]; // 8 notes
    } else if (rhythmRand < 0.75) {
      rhythmPattern = [0.25, 0.25, 0.5];
    } else {
      rhythmPattern = [0.5, 0.25, 0.25];
    }
  } else {
    // Medium: balanced
    if (rhythmRand < 0.20) {
      rhythmPattern = [0.25, 0.25, 0.25, 0.25];
    } else if (rhythmRand < 0.35) {
      rhythmPattern = [0.5, 0.5];
    } else if (rhythmRand < 0.45) {
      rhythmPattern = [1.0];
    } else if (rhythmRand < 0.55) {
      rhythmPattern = [0.5, 0.25, 0.25];
    } else if (rhythmRand < 0.65) {
      rhythmPattern = [0.25, 0.25, 0.5];
    } else if (rhythmRand < 0.75) {
      rhythmPattern = [0.75, 0.25];
    } else if (rhythmRand < 0.85) {
      rhythmPattern = [0.167, 0.167, 0.167, 0.167, 0.167, 0.165];
    } else {
      rhythmPattern = [0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125];
    }
  }
  
  // Decide if this beat will be a scale run (affected by movement)
  const isScaleRun = rhythmPattern.length >= 6 || (rhythmPattern.length >= 4 && seededRandom() < scaleProb);
  const scaleDirection = seededRandom() < 0.5 ? 1 : -1;
  
  for (let i = 0; i < rhythmPattern.length; i++) {
    const duration = Math.floor(beatDuration * rhythmPattern[i]);
    const r = seededRandom();
    
    let step;
    if (isScaleRun) {
      if (i === 0) {
        step = 0;
      } else if (r < 0.85) {
        step = scaleDirection;
      } else {
        step = -scaleDirection;
      }
    } else {
      // Movement affects hold probability
      if (r < 0.25) {
        step = 1;
      } else if (r < 0.50) {
        step = -1;
      } else if (r < 0.50 + holdProb) {
        step = 0; // Hold - affected by movement
      } else if (r < 0.70) {
        step = 2;
      } else if (r < 0.85) {
        step = -2;
      } else if (r < 0.92) {
        step = 3;
      } else {
        step = -3;
      }
    }
    
    currentDegree += step;
    
    // Keep in comfortable range
    if (currentDegree > chordDegree + 10) currentDegree -= 4;
    if (currentDegree < chordDegree - 3) currentDegree += 4;
    
    const note = getScaleNote(currentDegree, scale, baseOctave);
    if (note) {
      note.degree = currentDegree;
      
      // Add intervals - probability affected by movement
      let interval = null;
      if (!isScaleRun && seededRandom() < intervalProb && rhythmPattern[i] >= 0.5) {
        const intervalChoice = seededRandom();
        let intervalDegree;
        if (intervalChoice < 0.5) {
          intervalDegree = currentDegree - 2;
        } else if (intervalChoice < 0.8) {
          intervalDegree = currentDegree - 5;
        } else {
          intervalDegree = currentDegree - 7;
        }
        interval = getScaleNote(intervalDegree, scale, baseOctave);
      }
      
      events.push({ 
        note, 
        interval,
        duration,
        isLong: rhythmPattern[i] >= 0.5,
        isScale: isScaleRun
      });
    }
  }
  
  return events;
}

// Generate a complete waltz with proper timing and expression
function generateWaltz(bars = 8, scale = 'major', seed = Date.now(), tempo = 'slow', useTopLine = false, frolic = false) {
  // Seeded random for reproducibility
  let randomSeed = seed;
  const seededRandom = () => {
    randomSeed = (randomSeed * 9301 + 49297) % 233280;
    return randomSeed / 233280;
  };
  
  // Waltz timing: Viennese (fast) ~180 BPM, Slow waltz ~90 BPM
  // At 90 BPM: quarter note = 667ms, dotted half = 2000ms per bar
  // At 180 BPM: quarter note = 333ms, dotted half = 1000ms per bar
  const beatTiming = {
    'viennese': { beat1: 400, beat2: 300, beat3: 300 },  // Fast, energetic
    'slow': { beat1: 750, beat2: 550, beat3: 550 },      // Elegant, flowing
    'medium': { beat1: 550, beat2: 400, beat3: 400 }     // Moderate
  };
  
  const timing = beatTiming[tempo] || beatTiming.slow;
  
  const waltz = [];
  const progression = CHORD_PROGRESSIONS[Math.floor(seededRandom() * CHORD_PROGRESSIONS.length)];
  const melodyStyles = ['flowing', 'arpeggio', 'leap'];
  
  // Track last frolic note for continuity
  let lastFrolicNote = null;
  
  // Movement system - change character every 4-8 bars
  let currentMovement = null;
  let movementStartBar = 0;
  let movementLength = 0;
  let movementIndex = -1;
  const movements = []; // Track for display
  
  for (let bar = 0; bar < bars; bar++) {
    // Check if we need a new movement
    if (frolic && (bar === 0 || bar >= movementStartBar + movementLength)) {
      movementIndex = (movementIndex + 1) % MOVEMENTS.length;
      // Shuffle through movements but allow repeats after going through all
      if (bar === 0) {
        movementIndex = Math.floor(seededRandom() * MOVEMENTS.length);
      } else {
        // Pick a different movement than the current one
        let newIndex;
        do {
          newIndex = Math.floor(seededRandom() * MOVEMENTS.length);
        } while (newIndex === movementIndex && MOVEMENTS.length > 1);
        movementIndex = newIndex;
      }
      currentMovement = MOVEMENTS[movementIndex];
      movementStartBar = bar;
      movementLength = 4 + Math.floor(seededRandom() * 5); // 4-8 bars
      movements.push({ bar: bar + 1, movement: currentMovement, length: movementLength });
    }
    
    const chordDegree = progression[bar % progression.length];
    const chord = generateChord(chordDegree, scale, 4);
    const style = melodyStyles[Math.floor(seededRandom() * melodyStyles.length)];
    const melody = generateMelody(chordDegree, scale, style);
    
    // Safety: Ensure we have at least some notes
    if (melody.length === 0 || chord.length === 0) {
      console.error(`Warning: Empty melody or chord at bar ${bar + 1}`);
      continue; // Skip this bar
    }
    
    // Each bar is 3 beats (waltz time: STRONG-weak-weak)
    for (let beat = 0; beat < 3; beat++) {
      const melodyNote = melody[beat % melody.length];
      const bassNote = chord[0]; // Root note
      const beatDuration = beat === 0 ? timing.beat1 : (beat === 1 ? timing.beat2 : timing.beat3);
      
      // Generate optional top line ornamentation (original style)
      const topLineNote = useTopLine ? generateTopLine(chordDegree, scale, beat + 1, seededRandom) : null;
      
      // Generate frolic melody events - rhythm and notes that fill the beat
      let frolicEvents = null;
      if (frolic) {
        frolicEvents = generateFrolicEvents(chordDegree, scale, beat + 1, seededRandom, lastFrolicNote, beatDuration, currentMovement);
        // Track last note for continuity
        if (frolicEvents && frolicEvents.length > 0) {
          lastFrolicNote = frolicEvents[frolicEvents.length - 1].note;
        }
      }
      
      // Classic waltz: Bass on 1, chord on 2 & 3 (oom-pah-pah)
      let chordNotes, duration;
      if (beat === 0) {
        // Beat 1: Stronger, bass emphasis
        chordNotes = [chord[0]]; // Just root for bass
        duration = timing.beat1;
      } else {
        // Beats 2 & 3: Lighter, chord emphasis (oom-PAH-pah)
        // Use available chord notes, fallback to root if not enough
        chordNotes = [
          chord[1] || chord[0],
          chord[2] || chord[0]
        ];
        duration = beat === 1 ? timing.beat2 : timing.beat3;
      }
      
      waltz.push({
        melody: melodyNote,
        topLine: topLineNote,
        frolicEvents: frolicEvents,
        chord: chordNotes,
        bass: bassNote,
        duration,
        bar: bar + 1,
        beat: beat + 1,
        movement: currentMovement,
        isMovementStart: beat === 0 && bar === movementStartBar
      });
    }
  }
  
  return { waltz, movements };
}

// Percussion mapping for notepat
// Arrow keys, space, and alt keys make percussion sounds
const PERCUSSION = {
  kick: { key: 'ArrowDown', code: 'ArrowDown', keyCode: 40 },
  snare: { key: ' ', code: 'Space', keyCode: 32 },
  hihat: { key: 'ArrowUp', code: 'ArrowUp', keyCode: 38 },
  tom1: { key: 'ArrowLeft', code: 'ArrowLeft', keyCode: 37 },
  tom2: { key: 'ArrowRight', code: 'ArrowRight', keyCode: 39 },
  crash: { key: 'Alt', code: 'AltLeft', keyCode: 18 },
  ride: { key: 'Alt', code: 'AltRight', keyCode: 18 },
};

// Beat patterns for different feels
// Each beat is divided into 4 subdivisions (16th notes within the beat)
// Pattern: [[sub1, sub2, sub3, sub4], [sub1, sub2, sub3, sub4], [sub1, sub2, sub3, sub4]] for 3 beats
// Percussion sounds by Hz: kick(6k), hihat(8k), snare(2k), tom1(5k), tom2(7k), crash(3k), ride(4k)
const BEAT_PATTERNS = {
  waltz: [
    // Tight machine-like 16ths
    { name: 'machine', pattern: [
      [['kick', 'hihat'], ['hihat'], ['hihat'], ['hihat']],
      [['hihat'], ['hihat'], ['snare', 'hihat'], ['hihat']],
      [['hihat'], ['hihat'], ['hihat'], ['kick', 'hihat']]
    ]},
    // Breakbeat-influenced 
    { name: 'breakbeat', pattern: [
      [['kick'], [], ['kick'], ['hihat']],
      [[], ['snare'], [], ['hihat']],
      [['kick'], ['hihat'], ['snare'], []]
    ]},
    // Polyrhythmic feel - 3 against 4
    { name: 'poly', pattern: [
      [['kick', 'ride'], [], ['tom1'], []],
      [['tom2'], [], ['snare'], []],
      [['tom1'], [], ['kick'], ['hihat']]
    ]},
    // Fast hi-hat shuffle
    { name: 'shuffle', pattern: [
      [['kick', 'hihat'], ['hihat'], [], ['hihat']],
      [['hihat'], [], ['snare', 'hihat'], ['hihat']],
      [[], ['hihat'], ['hihat'], ['hihat']]
    ]},
    // Minimal techno pulse
    { name: 'minimal', pattern: [
      [['kick'], [], [], []],
      [[], [], ['snare'], []],
      [[], [], [], ['kick']]
    ]},
    // Four-on-floor with offbeat hats
    { name: 'disco', pattern: [
      [['kick'], [], ['hihat'], []],
      [['kick'], [], ['hihat', 'snare'], []],
      [['kick'], [], ['hihat'], []]
    ]},
    // Syncopated funk
    { name: 'funk', pattern: [
      [['kick'], ['hihat'], [], ['kick', 'hihat']],
      [[], ['snare', 'hihat'], ['hihat'], []],
      [['kick'], [], ['hihat'], ['snare']]
    ]},
    // Tom-heavy tribal
    { name: 'tribal', pattern: [
      [['kick', 'tom1'], [], ['tom2'], []],
      [['tom1'], [], ['tom2', 'kick'], []],
      [['tom1'], ['tom2'], ['tom1'], ['kick']]
    ]},
    // Jazzy brush feel
    { name: 'brushes', pattern: [
      [['kick'], [], [], ['ride']],
      [[], ['ride'], [], ['ride']],
      [[], [], ['snare'], ['ride']]
    ]},
    // Fast fills pattern
    { name: 'fills', pattern: [
      [['kick', 'crash'], ['tom1'], ['tom2'], ['tom1']],
      [['tom2'], ['tom1'], ['snare', 'tom2'], ['tom1']],
      [['tom2'], ['tom1'], ['tom2'], ['kick', 'crash']]
    ]},
  ]
};

async function pressPercussion(client, drum) {
  const perc = PERCUSSION[drum];
  if (!perc) return;
  
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: perc.key,
    code: perc.code,
    windowsVirtualKeyCode: perc.keyCode
  });
  await new Promise(r => setTimeout(r, 8));
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key: perc.key,
    code: perc.code,
    windowsVirtualKeyCode: perc.keyCode
  });
}

// Send a key event with velocity using Runtime.evaluate to dispatch a custom event
async function pressKeyWithVelocity(client, key, velocity = 127) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.charCodeAt(0);
  
  // Dispatch a custom KeyboardEvent with velocity property
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
        // Add velocity as a custom property
        event.velocity = ${velocity};
        window.dispatchEvent(event);
      })()
    `
  });
}

async function releaseKeyWithVelocity(client, key) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.charCodeAt(0);
  
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

async function pressKey(client, key) {
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    text: key,
    key: key,
    code: `Key${key.toUpperCase()}`,
    windowsVirtualKeyCode: key.charCodeAt(0)
  });
}

async function releaseKey(client, key) {
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyUp',
    key: key,
    code: `Key${key.toUpperCase()}`,
    windowsVirtualKeyCode: key.charCodeAt(0)
  });
}

// Press keys with velocity - for expressive playing
async function pressKeysWithVelocity(client, keys, duration, velocity = 127) {
  // Press all keys down simultaneously with velocity
  for (const key of keys) {
    await pressKeyWithVelocity(client, key, velocity);
    await new Promise(r => setTimeout(r, 5));
  }
  
  await new Promise(r => setTimeout(r, duration));
  
  // Release all keys
  for (const key of keys) {
    await releaseKeyWithVelocity(client, key);
  }
}

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
    await new Promise(r => setTimeout(r, 5));
  }
  
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

// Calculate swing-adjusted subdivision timings
// swingAmount: 50 = straight, 66 = triplet feel, 75+ = heavy swing
// Returns array of 4 timing values for each subdivision
function getSwingTimings(totalDuration, swingAmount) {
  // Swing shifts timing from the offbeats (2nd and 4th 16ths) 
  // At 50% swing, all subdivisions are equal (totalDuration/4 each)
  // At higher swing, 1st and 3rd are longer, 2nd and 4th are shorter
  const swingRatio = swingAmount / 100; // 0.5 = straight, 0.66 = triplet, etc.
  const baseTime = totalDuration / 4;
  
  // The "on" beats (1 and 3) get longer, "off" beats (2 and 4) get shorter
  const onBeatTime = baseTime * (swingRatio * 2);  // At 50%: 1.0x, at 66%: 1.32x
  const offBeatTime = baseTime * ((1 - swingRatio) * 2); // At 50%: 1.0x, at 66%: 0.68x
  
  return [onBeatTime, offBeatTime, onBeatTime, offBeatTime];
}

async function playGeneratedWaltz(client, waltz, title, tempo, hasFrolic = false, movements = [], hasBeat = false, swingAmount = 50, dynamicSwing = false) {
  const modeText = hasFrolic ? ', frolic mode' : '';
  const beatText = hasBeat ? ', with drums' : '';
  const swingText = dynamicSwing ? ', dynamic swing' : (swingAmount !== 50 ? `, swing=${swingAmount}%` : '');
  playingLog(`Now playing: ${title} (${waltz.length / 3} bars, ${waltz.length} beats, ${tempo} tempo${modeText}${beatText}${swingText})`);
  
  // Show movement overview if frolic mode
  if (hasFrolic && movements.length > 0) {
    console.log('');
    console.log(`${CYAN}┌─ Movements ─────────────────────────────────────────┐${RESET}`);
    for (const m of movements) {
      console.log(`${CYAN}│${RESET} ${m.movement.color}■${RESET} Bar ${m.bar}: ${m.movement.color}${m.movement.name}${RESET} (${m.length} bars)`);
    }
    console.log(`${CYAN}└─────────────────────────────────────────────────────┘${RESET}`);
  }
  console.log('');
  
  // Pick a random beat pattern to start
  const beatPatterns = BEAT_PATTERNS.waltz;
  let patternIndex = Math.floor(Math.random() * beatPatterns.length);
  let currentBeatPattern = beatPatterns[patternIndex].pattern;
  let currentPatternName = beatPatterns[patternIndex].name;
  
  // Swing state - can change dynamically
  let currentSwing = swingAmount;
  
  if (hasBeat) {
    const swingDesc = currentSwing === 50 ? 'straight' : 
                      currentSwing <= 60 ? 'light swing' :
                      currentSwing <= 70 ? 'triplet feel' : 'heavy swing';
    console.log(`${YELLOW}🥁 Beat: ${currentPatternName} | Swing: ${currentSwing}% (${swingDesc})${RESET}\n`);
  }
  
  let beatsPlayed = 0;
  let currentBar = 0;
  let currentMovementName = '';
  
  for (const measure of waltz) {
    // Dynamic swing: drift the swing amount slightly each bar
    if (dynamicSwing && measure.bar && measure.bar !== currentBar) {
      // Drift swing by -5 to +5, keeping it in range 40-85
      currentSwing += (Math.random() - 0.5) * 10;
      currentSwing = Math.max(40, Math.min(85, currentSwing));
    }
    
    // Show bar marker at the start of each new bar
    if (measure.bar && measure.bar !== currentBar) {
      // Check for movement change
      if (hasFrolic && measure.isMovementStart && measure.movement) {
        const m = measure.movement;
        console.log(`\n${m.color}╔═══════════════════════════════════════════════════════╗${RESET}`);
        console.log(`${m.color}║  ✦ ${m.name.toUpperCase()} ✦${RESET}`);
        console.log(`${m.color}╚═══════════════════════════════════════════════════════╝${RESET}`);
        currentMovementName = m.name;
        // Change beat pattern with movement - pick a new random one
        if (hasBeat) {
          patternIndex = Math.floor(Math.random() * beatPatterns.length);
          currentBeatPattern = beatPatterns[patternIndex].pattern;
          currentPatternName = beatPatterns[patternIndex].name;
          const swingDesc = currentSwing <= 55 ? 'straight' : 
                            currentSwing <= 65 ? 'light' :
                            currentSwing <= 75 ? 'triplet' : 'heavy';
          console.log(`${YELLOW}🥁 Beat: ${currentPatternName} | Swing: ${Math.round(currentSwing)}% (${swingDesc})${RESET}`);
        }
      }
      console.log(`\n${PURPLE_BG}${WHITE_TEXT} ═══ Bar ${measure.bar} ═══ ${RESET}`);
      currentBar = measure.bar;
    }
    
    // In frolic mode, play melody and bass TOGETHER throughout the beat
    if (hasFrolic && measure.frolicEvents && measure.frolicEvents.length > 0) {
      // Check if this is a scale run
      const isScaleRun = measure.frolicEvents[0]?.isScale;
      
      // Display frolic notes - compact for scale runs
      let frolicDisplay;
      if (isScaleRun && measure.frolicEvents.length >= 6) {
        // Show scale run compactly: first→last
        const first = measure.frolicEvents[0].note;
        const last = measure.frolicEvents[measure.frolicEvents.length - 1].note;
        frolicDisplay = `${GREEN}≋${first.note.toUpperCase()}${first.octave}→${last.note.toUpperCase()}${last.octave}${RESET}`;
      } else {
        frolicDisplay = measure.frolicEvents.map(e => {
          const noteStr = `~${e.note.note.toUpperCase()}${e.note.octave}`;
          const intervalStr = e.interval ? `+${e.interval.note.toUpperCase()}${e.interval.octave}` : '';
          const durStr = e.isLong ? '—' : '';
          return `${GREEN}${noteStr}${intervalStr}${durStr}${RESET}`;
        }).join(' ');
      }
      
      // Bass display
      const bassDisplay = measure.bass ? `${MAGENTA}${measure.bass.note.toUpperCase()}${measure.bass.octave}${RESET}` : '';
      const chordDisplay = measure.chord ? measure.chord.map(c => 
        `${CYAN}${c.note.toUpperCase()}${c.octave}${RESET}`
      ).join(' ') : '';
      
      // Get percussion subdivisions for this beat (4 subdivisions per beat)
      const beatIndex = measure.beat - 1; // 0, 1, or 2
      const percussionSubs = hasBeat ? (currentBeatPattern[beatIndex] || [[], [], [], []]) : [[], [], [], []];
      
      // Count total hits for display
      const totalHits = percussionSubs.flat().length;
      const percDisplay = totalHits > 0 ? ` 🥁×${totalHits}` : '';
      
      const beatMarker = measure.beat === 1 ? '♪♪' : '♪';
      process.stdout.write(`  ${beatMarker} [${frolicDisplay}] / [${bassDisplay} ${chordDisplay}]${percDisplay} `);
      
      // Velocity settings for expression
      const bassVelocity = 85 + Math.floor(Math.random() * 15); // 85-100
      const chordVelocity = 60 + Math.floor(Math.random() * 20); // 60-80
      
      // Play bass note at the start of beat (press and hold) with velocity
      const bassKeys = [];
      if (measure.bass) {
        const bassKey = noteToKey(measure.bass.note, measure.bass.octave);
        bassKeys.push(bassKey);
        await pressKeyWithVelocity(client, bassKey, bassVelocity);
      }
      
      // On beats 2 & 3, also press chord notes with softer velocity
      if (measure.beat !== 1 && measure.chord) {
        for (const chordNote of measure.chord) {
          const chordKey = noteToKey(chordNote.note, chordNote.octave);
          bassKeys.push(chordKey);
          await pressKeyWithVelocity(client, chordKey, chordVelocity);
        }
      }
      
      // Calculate subdivision timing
      const totalBeatDuration = measure.frolicEvents.reduce((sum, e) => sum + e.duration, 0) + measure.frolicEvents.length * 5;
      const subDivisionTime = totalBeatDuration / 4;
      
      // Interleave percussion subdivisions with melody
      let elapsedTime = 0;
      let subIndex = 0;
      
      // Fire first subdivision immediately
      if (hasBeat && percussionSubs[0] && percussionSubs[0].length > 0) {
        await Promise.all(percussionSubs[0].map(drum => pressPercussion(client, drum)));
      }
      
      // Now play the frolic melody events with percussion subdivisions
      let melodyNoteIndex = 0;
      for (const event of measure.frolicEvents) {
        const frolicKeys = [noteToKey(event.note.note, event.note.octave)];
        if (event.interval) {
          frolicKeys.push(noteToKey(event.interval.note, event.interval.octave));
        }
        
        // Melody velocity: varies with expression (accent first note of beat, randomize others)
        const isFirstNote = melodyNoteIndex === 0;
        const melodyVelocity = isFirstNote 
          ? 95 + Math.floor(Math.random() * 32) // 95-127 for accented notes
          : 70 + Math.floor(Math.random() * 40); // 70-110 for other notes
        melodyNoteIndex++;
        
        // Press frolic notes with velocity
        for (const key of frolicKeys) {
          await pressKeyWithVelocity(client, key, melodyVelocity);
        }
        
        // Hold for the event duration, firing percussion subdivisions as needed
        const eventEnd = elapsedTime + event.duration;
        while (elapsedTime < eventEnd) {
          const nextSubTime = (subIndex + 1) * subDivisionTime;
          const sleepTime = Math.min(eventEnd - elapsedTime, nextSubTime - elapsedTime);
          
          if (sleepTime > 0) {
            await new Promise(r => setTimeout(r, sleepTime));
            elapsedTime += sleepTime;
          }
          
          // Check if we crossed into a new subdivision
          const newSubIndex = Math.floor(elapsedTime / subDivisionTime);
          if (newSubIndex > subIndex && newSubIndex < 4) {
            subIndex = newSubIndex;
            if (hasBeat && percussionSubs[subIndex] && percussionSubs[subIndex].length > 0) {
              await Promise.all(percussionSubs[subIndex].map(drum => pressPercussion(client, drum)));
            }
          }
        }
        
        // Release frolic notes
        for (const key of frolicKeys) {
          await releaseKeyWithVelocity(client, key);
        }
        
        elapsedTime += 5;
        await new Promise(r => setTimeout(r, 5));
      }
      
      // Release bass/chord notes
      for (const key of bassKeys) {
        await releaseKeyWithVelocity(client, key);
      }
      
    } else {
      // Non-frolic mode: original behavior
      const keys = [];
      let displayNotes = [];
      
      // Top line (ornamental) - shown first, skip if null
      if (measure.topLine && measure.topLine.note) {
        const topKey = noteToKey(measure.topLine.note, measure.topLine.octave);
        keys.push(topKey);
        displayNotes.push(`${GREEN}✨${measure.topLine.note.toUpperCase()}${measure.topLine.octave}${RESET}`);
      }
      
      // Main melody
      if (measure.melody) {
        const melodyKey = noteToKey(measure.melody.note, measure.melody.octave);
        keys.push(melodyKey);
        displayNotes.push(`${YELLOW}${measure.melody.note.toUpperCase()}${measure.melody.octave}${RESET}`);
      }
      
      // Bass note
      if (measure.bass) {
        const bassKey = noteToKey(measure.bass.note, measure.bass.octave);
        keys.push(bassKey);
        displayNotes.push(`${MAGENTA}${measure.bass.note.toUpperCase()}${measure.bass.octave}${RESET}`);
      }
      
      // Chord notes
      if (measure.chord) {
        for (const chordNote of measure.chord) {
          const chordKey = noteToKey(chordNote.note, chordNote.octave);
          keys.push(chordKey);
          displayNotes.push(`${CYAN}${chordNote.note.toUpperCase()}${chordNote.octave}${RESET}`);
        }
      }
      
      // Get percussion subdivisions for this beat (non-frolic mode)
      const beatIndex = measure.beat - 1;
      const percussionSubs = hasBeat ? (currentBeatPattern[beatIndex] || [[], [], [], []]) : [[], [], [], []];
      const totalHits = percussionSubs.flat().length;
      const percDisplay = totalHits > 0 ? ` 🥁×${totalHits}` : '';
      
      // Show beat emphasis visually
      const beatMarker = measure.beat === 1 ? '♪♪' : '♪';
      process.stdout.write(`  ${beatMarker} [${displayNotes.join(' ')}]${percDisplay} `);
      
      // Calculate swing-adjusted subdivision timings
      const swingTimings = getSwingTimings(measure.duration, currentSwing);
      
      // Velocity for non-frolic mode
      const noteVelocity = 90 + Math.floor(Math.random() * 37); // 90-127
      
      // Press all melody/chord keys with velocity
      for (const key of keys) {
        await pressKeyWithVelocity(client, key, noteVelocity);
        await new Promise(r => setTimeout(r, 5));
      }
      
      // Play 4 subdivisions with percussion using swing timings
      for (let sub = 0; sub < 4; sub++) {
        // Fire percussion for this subdivision
        if (hasBeat && percussionSubs[sub] && percussionSubs[sub].length > 0) {
          await Promise.all(percussionSubs[sub].map(drum => pressPercussion(client, drum)));
        }
        await new Promise(r => setTimeout(r, swingTimings[sub]));
      }
      
      // Release all keys
      for (const key of keys) {
        await releaseKeyWithVelocity(client, key);
      }
    }
    
    beatsPlayed++;
    
    // Line break after each bar (3 beats)
    if (beatsPlayed % 3 === 0) {
      console.log('');
    }
    
    await new Promise(r => setTimeout(r, 30));
  }
  
  if (beatsPlayed % 3 !== 0) {
    console.log('');
  }
  
  console.log('');
  return beatsPlayed;
}

async function testGenerativeWaltz() {
  try {
    console.log('');
    testLog(`Starting Generative Waltz Composer`);
    console.log('');
    
    // Parse command line args: bars scale seed tempo [topline] [infinite] [frolic] [beat] [swing=N]
    const bars = parseInt(process.argv[2]) || 8;
    const baseScale = process.argv[3] || 'major';
    let seed = parseInt(process.argv[4]) || Date.now();
    const tempo = process.argv[5] || 'slow';
    const args = process.argv.slice(6).join(' ');
    const useTopLine = args.includes('topline');
    const infinite = args.includes('infinite');
    const useFrolic = args.includes('frolic');
    const useBeat = args.includes('beat');
    
    // Parse swing amount (0-100, where 50 is straight, 66 is triplet swing, 75 is heavy swing)
    // swing=0 means straight, swing=66 means triplet feel, swing=dynamic means vary during playback
    let swingAmount = 50; // Default: straight timing
    let dynamicSwing = false;
    const swingMatch = args.match(/swing=(\w+)/);
    if (swingMatch) {
      if (swingMatch[1] === 'dynamic') {
        dynamicSwing = true;
        swingAmount = 50 + Math.random() * 30; // Start somewhere between 50-80
      } else {
        swingAmount = parseInt(swingMatch[1]) || 50;
      }
    }
    
    // Ensure panel is open
    await Artery.openPanelStandalone();
    await new Promise(resolve => setTimeout(resolve, 500));
    
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC');
    
    await client.jump('notepat');
    testLog('Navigated to notepat');
    
    await new Promise(r => setTimeout(r, 2000));
    
    client.close();
    await client.connect();
    testLog('Reconnected after navigation');
    await new Promise(r => setTimeout(r, 500));
    
    await client.activateAudio();
    testLog('Audio context activated');
    
    console.log('');
    console.log(`${CYAN}${'='.repeat(60)}${RESET}`);
    console.log('');
    let legend;
    if (useFrolic) {
      legend = `${GREEN}~=Frolic (fast), ${YELLOW}Yellow=Melody, ${CYAN}Cyan=Chord, ${MAGENTA}Magenta=Bass${RESET}`;
    } else if (useTopLine) {
      legend = `${GREEN}✨=Top Line, ${YELLOW}Yellow=Melody, ${CYAN}Cyan=Chord, ${MAGENTA}Magenta=Bass${RESET}`;
    } else {
      legend = `${YELLOW}Yellow=Melody, ${CYAN}Cyan=Chord, ${MAGENTA}Magenta=Bass${RESET}`;
    }
    if (useBeat) {
      legend += ` ${WHITE_TEXT}| 🥁=Drums${RESET}`;
    }
    console.log(legend);
    if (infinite) {
      console.log(`${CYAN}🔄 Infinite mode: Press Ctrl+C to stop${RESET}`);
    }
    if (useFrolic) {
      console.log(`${GREEN}🌙 Frolic mode: Wandering sleepwalky melody over steady bass${RESET}`);
    }
    if (useBeat) {
      console.log(`${WHITE_TEXT}🥁 Beat mode: Percussion accompaniment (↓=kick ↑=hi-hat ␣=snare ←→=toms)${RESET}`);
    }
    console.log('');
    
    const scales = ['major', 'minor', 'dorian'];
    let waltzCount = 0;
    
    do {
      waltzCount++;
      
      // Evolve the musical parameters each iteration
      const scale = infinite ? scales[waltzCount % scales.length] : baseScale;
      const currentBars = infinite ? (4 + (waltzCount % 3) * 4) : bars; // Vary 4, 8, 12 bars
      seed = infinite ? seed + waltzCount * 111 : seed;
      
      const topLineMsg = useTopLine ? ' with top line' : '';
      const frolicMsg = useFrolic ? ' (frolic mode)' : '';
      const beatMsg = useBeat ? ' (with beat)' : '';
      const swingMsg = dynamicSwing ? ' (dynamic swing)' : (swingAmount !== 50 ? ` (swing=${swingAmount})` : '');
      playingLog(`Waltz #${waltzCount}: ${currentBars} bars in ${scale} (seed: ${seed}, ${tempo}${topLineMsg}${frolicMsg}${beatMsg}${swingMsg})`);
      
      const { waltz, movements } = generateWaltz(currentBars, scale, seed, tempo, useTopLine, useFrolic);
      
      const beatsPlayed = await playGeneratedWaltz(
        client, 
        waltz, 
        `Generative Waltz #${waltzCount} in ${scale.charAt(0).toUpperCase() + scale.slice(1)}`,
        tempo,
        useFrolic,
        movements,
        useBeat,
        swingAmount,
        dynamicSwing
      );
      
      console.log(`${CYAN}${'='.repeat(60)}${RESET}`);
      console.log('');
      
      const totalTime = (waltz.reduce((sum, w) => sum + w.duration, 0) / 1000).toFixed(1);
      successLog(`Waltz #${waltzCount} complete! ${Math.floor(beatsPlayed / 3)} bars, ${beatsPlayed} beats in ${totalTime}s`);
      
      if (infinite) {
        console.log(`${CYAN}💫 Generating next waltz...${RESET}`);
        console.log('');
        await new Promise(r => setTimeout(r, 500));
      }
      
    } while (infinite);
    
    if (!infinite) {
      let regenCmd = `test-generative-waltz ${bars} ${baseScale} ${seed} ${tempo}`;
      if (useTopLine) regenCmd += ' topline';
      if (useFrolic) regenCmd += ' frolic';
      if (useBeat) regenCmd += ' beat';
      console.log(`${CYAN}💡 Regenerate with same seed: ${regenCmd}${RESET}`);
      console.log('');
    }
    
    await client.jump('prompt');
    testLog('Returned to prompt');
    await new Promise(r => setTimeout(r, 500));
    
    await Artery.closePanelStandalone();
    
    client.close();
    process.exit(0);
    
  } catch (error) {
    console.error(`💔 Generation failed: ${error.message}`);
    console.error(error.stack);
    process.exit(1);
  }
}

console.log(`${CYAN}🎵 Generative Waltz Composer${RESET}`);
console.log(`${CYAN}Usage: test-generative-waltz [bars] [scale] [seed] [tempo] [topline] [infinite] [frolic] [beat] [swing=N|dynamic]${RESET}`);
console.log(`${CYAN}  bars: number of bars (default: 8)${RESET}`);
console.log(`${CYAN}  scale: major, minor, dorian (default: major)${RESET}`);
console.log(`${CYAN}  seed: number for reproducible generation${RESET}`);
console.log(`${CYAN}  tempo: viennese, medium, slow (default: slow)${RESET}`);
console.log(`${CYAN}  topline: add ornamental melody (optional)${RESET}`);
console.log(`${CYAN}  infinite: continuously generate evolving waltzes (optional)${RESET}`);
console.log(`${CYAN}  frolic: wandering sleepwalky melody over steady bass (optional)${RESET}`);
console.log(`${CYAN}  beat: add percussion accompaniment (optional)${RESET}`);
console.log(`${CYAN}  swing=N: set swing amount 50-85 (50=straight, 66=triplet, 75+=heavy)${RESET}`);
console.log(`${CYAN}  swing=dynamic: swing drifts during playback${RESET}`);
console.log('');

testGenerativeWaltz();
