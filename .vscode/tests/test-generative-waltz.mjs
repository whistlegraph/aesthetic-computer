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

// Generate a complete waltz with proper timing and expression
function generateWaltz(bars = 8, scale = 'major', seed = Date.now(), tempo = 'slow', useTopLine = false) {
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
  
  for (let bar = 0; bar < bars; bar++) {
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
      
      // Generate optional top line ornamentation
      const topLineNote = useTopLine ? generateTopLine(chordDegree, scale, beat + 1, seededRandom) : null;
      
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
        chord: chordNotes,
        bass: bassNote,
        duration,
        bar: bar + 1,
        beat: beat + 1
      });
    }
  }
  
  return waltz;
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

async function playGeneratedWaltz(client, waltz, title, tempo) {
  playingLog(`Now playing: ${title} (${waltz.length / 3} bars, ${waltz.length} beats, ${tempo} tempo)`);
  console.log('');
  
  let beatsPlayed = 0;
  let currentBar = 0;
  
  for (const measure of waltz) {
    // Show bar marker at the start of each new bar
    if (measure.bar && measure.bar !== currentBar) {
      console.log(`\n${PURPLE_BG}${WHITE_TEXT} ═══ Bar ${measure.bar} ═══ ${RESET}`);
      currentBar = measure.bar;
    }
    
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
    
    // Show beat emphasis visually
    const beatMarker = measure.beat === 1 ? '♪♪' : '♪';
    process.stdout.write(`  ${beatMarker} [${displayNotes.join(' ')}] `);
    
    await pressKeys(client, keys, measure.duration);
    
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
    
    // Parse command line args: bars scale seed tempo [topline] [infinite]
    const bars = parseInt(process.argv[2]) || 8;
    const baseScale = process.argv[3] || 'major';
    let seed = parseInt(process.argv[4]) || Date.now();
    const tempo = process.argv[5] || 'slow';
    const useTopLine = process.argv[6] === 'topline' || process.argv[7] === 'topline';
    const infinite = process.argv[6] === 'infinite' || process.argv[7] === 'infinite';
    
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
    const legend = useTopLine 
      ? `${GREEN}✨=Top Line, ${YELLOW}Yellow=Melody, ${CYAN}Cyan=Chord, ${MAGENTA}Magenta=Bass${RESET}`
      : `${YELLOW}Yellow=Melody, ${CYAN}Cyan=Chord, ${MAGENTA}Magenta=Bass${RESET}`;
    console.log(legend);
    if (infinite) {
      console.log(`${CYAN}🔄 Infinite mode: Press Ctrl+C to stop${RESET}`);
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
      playingLog(`Waltz #${waltzCount}: ${currentBars} bars in ${scale} (seed: ${seed}, ${tempo}${topLineMsg})`);
      
      const waltz = generateWaltz(currentBars, scale, seed, tempo, useTopLine);
      
      const beatsPlayed = await playGeneratedWaltz(
        client, 
        waltz, 
        `Generative Waltz #${waltzCount} in ${scale.charAt(0).toUpperCase() + scale.slice(1)}`,
        tempo
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
      const regenCmd = useTopLine 
        ? `test-generative-waltz ${bars} ${baseScale} ${seed} ${tempo} topline`
        : `test-generative-waltz ${bars} ${baseScale} ${seed} ${tempo}`;
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
console.log(`${CYAN}Usage: test-generative-waltz [bars] [scale] [seed] [tempo] [topline] [infinite]${RESET}`);
console.log(`${CYAN}  bars: number of bars (default: 8)${RESET}`);
console.log(`${CYAN}  scale: major, minor, dorian (default: major)${RESET}`);
console.log(`${CYAN}  seed: number for reproducible generation${RESET}`);
console.log(`${CYAN}  tempo: viennese, medium, slow (default: slow)${RESET}`);
console.log(`${CYAN}  topline: add ornamental melody (optional)${RESET}`);
console.log(`${CYAN}  infinite: continuously generate evolving waltzes (optional)${RESET}`);
console.log('');

testGenerativeWaltz();
