/**
 * Notepat Fuzzing Algorithm
 *
 * A generative musical composition algorithm for the notepat piece.
 * Randomly explores octaves, wavetypes, and scale patterns.
 *
 * Usage:
 * 1. Navigate to aesthetic.computer
 * 2. Type 'notepat' in the prompt
 * 3. Paste this script into the browser console
 * 4. Call startNotepatFuzz() to begin
 * 5. Call window.stopNotepat() to stop
 */

const notes = ['c', 'd', 'e', 'f', 'g', 'a', 'b', 'h', 'i', 'j', 'k', 'l', 'm', 'n'];
const octaves = ['3', '4', '5', '6', '7', '8'];

/**
 * Dispatch a keyboard event (both keydown and keyup)
 */
async function pressKey(key, code, keyCode, holdDuration = 30) {
  document.dispatchEvent(new KeyboardEvent('keydown', {
    key: key,
    code: code,
    keyCode: keyCode,
    bubbles: true,
    cancelable: true
  }));

  await new Promise(r => setTimeout(r, holdDuration));

  document.dispatchEvent(new KeyboardEvent('keyup', {
    key: key,
    code: code,
    keyCode: keyCode,
    bubbles: true,
    cancelable: true
  }));
}

/**
 * Change to a random octave
 */
async function randomOctave() {
  const octave = octaves[Math.floor(Math.random() * octaves.length)];
  await pressKey(octave, `Digit${octave}`, 48 + parseInt(octave));
  await new Promise(r => setTimeout(r, 50));
  return octave;
}

/**
 * Change wavetype by pressing Tab
 */
async function changeWavetype() {
  await pressKey('Tab', 'Tab', 9);
  await new Promise(r => setTimeout(r, 50));
}

/**
 * Play a single note with randomized duration
 */
async function playNote(note) {
  const holdDuration = 20 + Math.random() * 40; // 20-60ms
  await pressKey(note, `Key${note.toUpperCase()}`, note.charCodeAt(0), holdDuration);

  const gap = 10 + Math.random() * 30; // 10-40ms gap
  await new Promise(r => setTimeout(r, gap));
}

/**
 * Play a random sequence of notes from the scale
 */
async function playPhrase(running) {
  // Randomly go up or down the scale
  const goingUp = Math.random() > 0.5;
  const scaleNotes = goingUp ? notes : [...notes].reverse();

  // Pick a random segment of the scale (3-8 notes)
  const numNotes = 3 + Math.floor(Math.random() * 6);
  const startIdx = Math.floor(Math.random() * (scaleNotes.length - numNotes));
  const notesToPlay = scaleNotes.slice(startIdx, startIdx + numNotes);

  // Play each note in the sequence
  for (const note of notesToPlay) {
    if (!running.value) break;
    await playNote(note);
  }

  return notesToPlay.length;
}

/**
 * Main fuzzing loop
 */
async function startNotepatFuzz() {
  const running = { value: true };
  let totalNotes = 0;
  let totalPhrases = 0;
  let octaveChanges = 0;
  let wavetypeChanges = 0;

  // Set up stop function
  window.stopNotepat = () => {
    running.value = false;
    console.log('Stopping notepat fuzzer...');
    console.log(`Final stats: ${totalPhrases} phrases, ${totalNotes} notes, ${octaveChanges} octave changes, ${wavetypeChanges} wavetype changes`);
  };

  console.log('Starting notepat fuzzer... (call window.stopNotepat() to stop)');

  while (running.value) {
    // Change to a random octave
    const octave = await randomOctave();
    octaveChanges++;

    // Maybe change wavetype (50% chance)
    if (Math.random() > 0.5) {
      await changeWavetype();
      wavetypeChanges++;
    }

    // Play a phrase
    const notesPlayed = await playPhrase(running);
    totalNotes += notesPlayed;
    totalPhrases++;

    // Log progress every 10 phrases
    if (totalPhrases % 10 === 0) {
      console.log(`Progress: ${totalPhrases} phrases, ${totalNotes} notes, octave=${octave}`);
    }
  }

  return {
    phrases: totalPhrases,
    notes: totalNotes,
    octaveChanges,
    wavetypeChanges
  };
}

/**
 * Start with limited iterations (for testing)
 */
async function startNotepatFuzzLimited(iterations = 10) {
  let totalNotes = 0;
  let octaveChanges = 0;
  let wavetypeChanges = 0;

  for (let i = 0; i < iterations; i++) {
    await randomOctave();
    octaveChanges++;

    if (Math.random() > 0.5) {
      await changeWavetype();
      wavetypeChanges++;
    }

    const running = { value: true };
    const notesPlayed = await playPhrase(running);
    totalNotes += notesPlayed;
  }

  console.log(`Played ${totalNotes} notes with ${octaveChanges} octave changes and ${wavetypeChanges} wavetype changes`);
  return { totalNotes, octaveChanges, wavetypeChanges };
}

// Export functions for use
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    startNotepatFuzz,
    startNotepatFuzzLimited,
    notes,
    octaves
  };
}
