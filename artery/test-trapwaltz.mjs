#!/usr/bin/env node
/**
 * Trap Waltz Generator
 * 
 * Combines classical waltz elegance (3/4 time) with trap production
 * Rolling hats, syncopated kicks, but in waltz time signature
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

// Chord progressions in waltz style (8 bars)
const PROGRESSIONS = {
  'classical': [
    [0, 3, 4, 4], // i - iv - V - V
    [0, 5, 3, 4], // i - vi - iv - V
  ],
  'dark': [
    [0, 5, 6, 4], // i - VI - VII - V (borrowed chords)
    [0, 3, 6, 4], // i - iv - VII - V
  ],
  'romantic': [
    [0, 3, 4, 0], // i - iv - V - i
    [0, 2, 4, 0], // i - iii - V - i
  ],
  'trap': [
    [0, 6, 3, 4], // i - VII - iv - V
    [0, 0, 3, 4], // i - i - iv - V (emphasis on tonic)
  ],
};

// Note names for display
const NOTE_NAMES = ['c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'];

// Get note from scale degree
function getScaleNote(degree, scale = 'minor', rootNote = 'a', octave = 4) {
  const scaleIntervals = SCALES[scale];
  const noteIndex = ((degree % 7) + 7) % 7; // Handle negative
  const octaveShift = Math.floor(degree / 7);
  const semitones = scaleIntervals[noteIndex];
  
  // Get root note index
  const rootIndex = NOTE_NAMES.indexOf(rootNote.toLowerCase());
  const chromaticIndex = (rootIndex + semitones) % 12;
  
  const note = NOTE_NAMES[chromaticIndex].replace('#', 's'); // ac uses 'cs' not 'c#'
  return { note, octave: octave + octaveShift };
}

// Generate waltz bass pattern (oom-pah-pah style but with trap character)
function generateWaltzBass(chordDegree, scale, rootNote, seededRandom, style = 'classic') {
  const bass = [];
  const octave = 2;
  
  if (style === 'classic' || style === 'baroque') {
    // Traditional oom-pah-pah
    bass.push({ beat: 1, note: getScaleNote(chordDegree, scale, rootNote, octave), duration: 'quarter' });
    bass.push({ beat: 2, note: getScaleNote(chordDegree + 2, scale, rootNote, octave + 1), duration: 'quarter' });
    bass.push({ beat: 3, note: getScaleNote(chordDegree + 4, scale, rootNote, octave + 1), duration: 'quarter' });
  } else if (style === 'dark' || style === 'drill') {
    // Just root, let it ring
    bass.push({ beat: 1, note: getScaleNote(chordDegree, scale, rootNote, octave), duration: 'half' });
  } else if (style === 'phonk') {
    // Bouncy bass
    bass.push({ beat: 1, note: getScaleNote(chordDegree, scale, rootNote, octave), duration: 'eighth' });
    bass.push({ beat: 1.5, note: getScaleNote(chordDegree, scale, rootNote, octave), duration: 'eighth' });
    bass.push({ beat: 3, note: getScaleNote(chordDegree - 2, scale, rootNote, octave), duration: 'quarter' });
  } else {
    // Minimal
    bass.push({ beat: 1, note: getScaleNote(chordDegree, scale, rootNote, octave), duration: 'dotted-half' });
  }
  
  return bass;
}

// Generate melodic content - elegant but with trap flair
function generateMelody(chordDegree, scale, rootNote, seededRandom, style, bar) {
  const melody = [];
  const octave = 5;
  
  // Probability of playing based on style
  const densityMap = {
    'classic': 0.7,
    'dark': 0.5,
    'dreamy': 0.3,
    'baroque': 0.9,
    'minimal': 0.2,
    'phonk': 0.6,
    'viennese': 0.8,
    'drill': 0.4,
  };
  
  const density = densityMap[style] || 0.5;
  
  // Beat 1 - often play chord tone
  if (seededRandom() < density) {
    const degrees = [chordDegree + 4, chordDegree + 2, chordDegree]; // 5th, 3rd, root
    const degree = degrees[Math.floor(seededRandom() * degrees.length)];
    melody.push({ beat: 1, note: getScaleNote(degree, scale, rootNote, octave), duration: 'quarter' });
  }
  
  // Beat 2 - passing tones or rest
  if (seededRandom() < density * 0.7) {
    const degree = chordDegree + 1 + Math.floor(seededRandom() * 4);
    melody.push({ beat: 2, note: getScaleNote(degree, scale, rootNote, octave), duration: 'eighth' });
  }
  
  // Beat 3 - anticipation or resolution
  if (seededRandom() < density * 0.8) {
    const degree = chordDegree + 2 + Math.floor(seededRandom() * 3);
    const duration = seededRandom() < 0.5 ? 'quarter' : 'eighth';
    melody.push({ beat: 3, note: getScaleNote(degree, scale, rootNote, octave), duration });
  }
  
  // Baroque style gets extra ornaments
  if (style === 'baroque' && seededRandom() < 0.5) {
    const degree = chordDegree + 3 + Math.floor(seededRandom() * 4);
    melody.push({ beat: 2.5, note: getScaleNote(degree, scale, rootNote, octave + 1), duration: 'sixteenth' });
  }
  
  return melody;
}

// Generate a complete trap waltz
function generateTrapWaltz(opts = {}) {
  const {
    bars = 8,
    bpm = 140,
    style = 'classic',
    scale = 'minor',
    rootNote = 'a',
    seed = Date.now(),
    progression = 'classical',
  } = opts;
  
  let randomSeed = seed;
  const seededRandom = () => {
    randomSeed = (randomSeed * 9301 + 49297) % 233280;
    return randomSeed / 233280;
  };
  
  const drumPattern = TRAPWALTZ_PATTERNS[style] || TRAPWALTZ_PATTERNS['classic'];
  const chordProg = PROGRESSIONS[progression]?.[Math.floor(seededRandom() * 2)] || [0, 3, 4, 0];
  
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
      
      // Kick
      if (drumPattern.kick[step]) {
        events.push({
          time: stepTime,
          type: 'drum',
          note: 'c2',
          velocity: step === 0 ? 1.0 : 0.85,
          duration: msPerStep * 2,
        });
      }
      
      // Snare
      if (drumPattern.snare[step]) {
        const velocity = step === 4 ? 1.0 : 0.75; // Beat 2 is strongest
        events.push({
          time: stepTime,
          type: 'drum',
          note: 'd2',
          velocity,
          duration: msPerStep,
        });
      }
      
      // Hi-hat
      if (drumPattern.hat[step]) {
        // Trap-style velocity variation
        const accent = (step % 4 === 0) ? 0.9 : 0.5 + seededRandom() * 0.3;
        events.push({
          time: stepTime,
          type: 'drum',
          note: 'fs2', // f#2 for closed hat
          velocity: accent,
          duration: msPerStep * 0.5,
        });
      }
      
      // Open hat
      if (drumPattern.hatOpen?.[step]) {
        events.push({
          time: stepTime,
          type: 'drum',
          note: 'gs2', // g#2 for open hat
          velocity: 0.7,
          duration: msPerStep * 1.5,
        });
      }
    }
    
    // === BASS (waltz pattern) ===
    const bassNotes = generateWaltzBass(chordDegree, scale, rootNote, seededRandom, style);
    for (const bass of bassNotes) {
      const beatTime = barStart + ((bass.beat - 1) * msPerBeat);
      const duration = bass.duration === 'half' ? msPerBeat * 2 :
                       bass.duration === 'dotted-half' ? msPerBeat * 3 :
                       bass.duration === 'eighth' ? msPerBeat * 0.5 :
                       msPerBeat;
      events.push({
        time: beatTime,
        type: 'bass',
        note: `${bass.note.note}${bass.note.octave}`,
        velocity: 0.8,
        duration,
      });
    }
    
    // === MELODY ===
    const melodyNotes = generateMelody(chordDegree, scale, rootNote, seededRandom, style, bar);
    for (const mel of melodyNotes) {
      const beatTime = barStart + ((mel.beat - 1) * msPerBeat);
      const duration = mel.duration === 'half' ? msPerBeat * 2 :
                       mel.duration === 'quarter' ? msPerBeat :
                       mel.duration === 'eighth' ? msPerBeat * 0.5 :
                       msPerBeat * 0.25;
      events.push({
        time: beatTime,
        type: 'melody',
        note: `${mel.note.note}${mel.note.octave}`,
        velocity: 0.6 + seededRandom() * 0.2,
        duration,
      });
    }
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
        const key = noteToKey(event.note);
        
        if (key) {
          // Velocity affects duration
          const holdTime = Math.min(event.duration * event.velocity, 500);
          
          await client.send('Input.dispatchKeyEvent', {
            type: 'keyDown',
            key: key,
            code: `Key${key.toUpperCase()}`,
          });
          
          setTimeout(async () => {
            await client.send('Input.dispatchKeyEvent', {
              type: 'keyUp',
              key: key,
              code: `Key${key.toUpperCase()}`,
            });
          }, holdTime);
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
